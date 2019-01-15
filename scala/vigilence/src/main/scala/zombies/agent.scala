package zombies

import world._
import space._

import scala.util.Random

object agent {


  sealed trait Agent
  case class Human(position: Position, velocity: Velocity, speed: Speed, vision: Double, maxRotation: Double, followMode: FollowMode) extends Agent
  case class Zombie(position: Position, velocity: Velocity, speed: Speed, vision: Double, maxRotation: Double) extends Agent
  case class Speed(walkSpeed: Double, runSpeed: Double, maxStamina: Int, stamina: Int, run: Boolean)

  sealed trait FollowMode
  object NoFollow extends FollowMode
  case class FollowRunning(probability: Double) extends FollowMode

  object Agent {

    def isHuman(agent: Agent) = agent match {
      case _: Human => true
      case _ => false
    }

    def isZombie(agent: Agent) = agent match {
      case _: Zombie => true
      case _ => false
    }

    def zombify(zombie: Zombie, human: Human) = {
      zombie.copy(
        position = human.position,
        velocity = human.velocity,
        speed = zombie.speed.copy(stamina = 0))
    }


    def position(agent: Agent) = agent match {
      case h: Human => h.position
      case z: Zombie => z.position
    }

    def velocity(agent: Agent) = agent match {
      case h: Human => h.velocity
      case z: Zombie => z.velocity
    }

    def vision(agent: Agent) = agent match {
      case h: Human => h.vision
      case z: Zombie => z.vision
    }

    def location(agent: Agent, side: Int): Location = positionToLocation(position(agent), side, side)

    def index(agents: Vector[Agent], side: Int) = Index[Agent](agents, location(_, side), side)

    def randomPosition(world: World, rng: Random) = World.randomPosition(world, rng)
    def randomVelocity(maxSpeed: Double, rng: Random) = {
      val (x, y) = randomUnitVector(rng)
      normalize((x * 2 - 1, y * 2 - 1), maxSpeed)
    }


    def visibleNeighbors(index: Index[Agent], agent: Agent, range: Double, world: World) = {
      val neighborhoodSize = math.ceil(range / space.cellSide(index.side)).toInt
      val location = Agent.location(agent, index.side)
      shadow.visible(location, World.isWall(world, _, _), (index.side, index.side), neighborhoodSize).
        flatMap { case(x, y) => Index.get(index, y, y) }.
        filter(n => distance(Agent.position(n), Agent.position(agent)) < range)
    }

    def neighbors(index: Index[Agent], agent: Agent, range: Double) = {
      val neighborhoodSize = math.ceil(range / space.cellSide(index.side)).toInt
      val (x, y) = Agent.location(agent, index.side)
      space.neighbors(Index.get(index, _, _), x, y, neighborhoodSize).filter(n => distance(Agent.position(n), Agent.position(agent)) < range)
    }

    def neighbors(index: Index[Agent], agent: Agent, range: Double, neighborhood: NeighborhoodCache) = {
      val (x, y) = Agent.location(agent, index.side)
      neighborhood(x)(y).
        flatMap { case(x, y) => Index.get(index, x, y) }.
        filter(n => distance(Agent.position(n), Agent.position(agent)) < range)
    }

    def projectedVelocities(granularity: Int, maxRotation: Double, velocity: Velocity, speed: Double) =
      (-granularity to granularity).map(_ * maxRotation).map(r => normalize(rotate(velocity, r), speed))

    def towardsWall(world: World, position: Position, velocity: Velocity) = {
      val (x, y) = space.positionToLocation(sum(position, velocity), world.side, world.side)
      World.get(world, x, y) match {
        case Some(Wall) => true
        case _ => false
      }
    }

    def move(agent: Agent, world: World, granularity: Int, rng: Random) = {

      def computeVelocity(position: Position, velocity: Velocity, maxRotation: Double, speed: Double) = {
        val (px, py) = sum(position, velocity)
        val (cx, cy) = positionToLocation((px, py), world.side, world.side)

        val newDirection =
          World.get(world, cx, cy) match {
            case None => None
            case Some(Wall) =>
              val velocities = projectedVelocities(granularity, maxRotation, velocity, speed)
              rng.shuffle(velocities).find(v => !towardsWall(world, position, v)) match {
                case Some(v) => Some(v)
                case None => Some(opposite(velocity))
              }
            case Some(f: Floor) => Some(sum(velocity, normalize((f.wallSlope.x, f.wallSlope.y), f.wallSlope.intensity * speed)))
          }

        newDirection.map(d => normalize(d, speed))
      }

      def computePosition(position: Position, velocity: Velocity) = {
        val newPosition = sum(position, velocity)
        val (px, py) = newPosition
        if (px < 0 || px > 1 || py < 0 || py > 1) None else Some(newPosition)
      }

      agent match {
        case h: Human =>
          for {
            v <- computeVelocity(h.position, h.velocity, h.maxRotation, Speed.effectiveSpeed(h.speed))
            p <- computePosition(h.position, v)
          } yield h.copy(position = p, velocity = v)
        case z: Zombie =>
          for {
            v <- computeVelocity(z.position, z.velocity, z.maxRotation, Speed.effectiveSpeed(z.speed))
            p <- computePosition(z.position, v)
          } yield z.copy(position = p, velocity = v)
        case a => Some(a)
      }

    }

    def metabolism(a: Agent) =
      a match  {
        case human: Human => Human.metabolism(human)
        case zombie: Zombie => Zombie.metabolism(zombie)
        case a => a
      }

    def adaptDirectionRotate(world: World, index: Index[Agent], agent: Agent, granularity: Int, neighborhoodCache: NeighborhoodCache, rng: Random) = {

      agent match {
        case h: Human =>
          val nhs =  neighbors(index, agent, vision(h), neighborhoodCache)
          (nhs.filter(Agent.isZombie), h.followMode) match {
            case (ns, _) if !ns.isEmpty =>
              val running = Human.run(h)
              val pv = projectedVelocities(granularity, running.maxRotation, running.velocity, Speed.effectiveSpeed(running.speed))
              val nv = rng.shuffle(pv.filter(pv => !towardsWall(world, h.position, pv)))
              if(nv.isEmpty) running else running.copy(velocity = nv.maxBy { v => ns.map(n => distance(position(n), sum(running.position, v))).min })
            case (_, followRunning: FollowRunning) =>
              val runningNeighbors = nhs.collect { case h: Human => h }.filter { _.speed.run }
              if(!runningNeighbors.isEmpty && rng.nextDouble() < followRunning.probability) Human.run(h.copy(velocity = average(runningNeighbors.map(_.velocity))))
              else h
            case _ => h
          }
        case z: Zombie =>
          neighbors(index, agent, vision(z), neighborhoodCache).filter(Agent.isHuman) match {
            case ns if !ns.isEmpty =>
              val running = Zombie.run(z)
              val pv =  projectedVelocities(granularity, running.maxRotation, running.velocity, Speed.effectiveSpeed(running.speed))
              val nv = rng.shuffle(pv.filter(pv => !towardsWall(world, z.position, pv)))
              if(nv.isEmpty) running else running.copy(velocity = nv.minBy { v => ns.map(n => distance(position(n), sum(running.position, v))).min })
            case _ => z
          }

      }
    }

    def infect(index: Index[Agent], agents: Vector[Agent], range: Double, zombify: (Zombie, Human) => Zombie) = {
      val (humansAgents, others) = agents.partition(Agent.isHuman)
      val humans = humansAgents.collect { case h: Human => h }
      humans.map {
        h =>
          attacker(index, h, range) match {
            case Some(z: Zombie) => zombify(z, h)
            case Some(a) => sys.error(s"Attacker is $a, this should never happen")
            case None => h
          }
      } ++ others
    }

    def attacker(index: Index[Agent], agent: Human, range: Double) =
      neighbors(index, agent, range).find(Agent.isZombie)

  }


  object Speed {
    def effectiveSpeed(speed: Speed) =  if(speed.run) speed.runSpeed else speed.walkSpeed
    def metabolism(speed: Speed) =
      (speed.run, speed.stamina > 0) match {
        case (false, _) if speed.stamina < speed.maxStamina => speed.copy(stamina = speed.stamina + 1)
        case (false, _) => speed
        case (true, true) => speed.copy(stamina = speed.stamina - 1)
        case (true, false) => speed.copy(stamina = 0, run = false)
      }
    def canRun(speed: Speed) = speed.stamina >= speed.maxStamina / 2
  }


  object Human {
    def random(world: World, walkSpeed: Double, runSpeed: Double, maxStamina: Int, vision: Double, maxRotation: Double, followMode: FollowMode, rng: Random) = {
      val p = Agent.randomPosition(world, rng)
      val v = Agent.randomVelocity(walkSpeed, rng)
      Human(p, v, Speed(walkSpeed, runSpeed, maxStamina, maxStamina, false), vision, maxRotation, followMode)
    }

    def run(h: Human) =
      if(Speed.canRun(h.speed)) h.copy(velocity = normalize(h.velocity, h.speed.runSpeed), speed = h.speed.copy(run = true))
      else h

    def metabolism(h: Human) = {
      val newSpeed = Speed.metabolism(h.speed)
      val newVelocity = normalize(h.velocity, Speed.effectiveSpeed(newSpeed))
      h.copy(velocity = newVelocity, speed = newSpeed)
    }
  }

  object Zombie {
    def random(world: World, walkSpeed: Double, runSpeed: Double, maxStamina: Int, vision: Double, maxRotation: Double, rng: Random) = {
      val p = Agent.randomPosition(world, rng)
      val v = Agent.randomVelocity(walkSpeed, rng)
      Zombie(p, v, Speed(walkSpeed, runSpeed, maxStamina, maxStamina, false), vision, maxRotation)
    }

    def run(z: Zombie) =
      if(Speed.canRun(z.speed)) z.copy(velocity = normalize(z.velocity, z.speed.runSpeed), speed = z.speed.copy(run = true))
      else z

    def metabolism(z: Zombie) = {
      val newSpeed = Speed.metabolism(z.speed)
      val newVelocity = normalize(z.velocity, Speed.effectiveSpeed(newSpeed))
      z.copy(velocity = newVelocity, speed = newSpeed)
    }

  }
}