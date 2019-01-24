package zombies

import world._
import space._

import scala.collection.mutable.ListBuffer
import scala.util.Random

object agent {


  sealed trait Agent
  case class Human(position: Position, velocity: Velocity, speed: Speed, vision: Double, maxRotation: Double, followRunningProbability: Double, rescue: Rescue) extends Agent
  case class Zombie(position: Position, velocity: Velocity, speed: Speed, vision: Double, maxRotation: Double) extends Agent
  case class Speed(walkSpeed: Double, runSpeed: Double, maxStamina: Int, stamina: Int, run: Boolean)

  case class Rescue(informed: Boolean = false, alerted: Boolean = false, perceiveInformation: Double = 0.0)

  object Agent {

    def isHuman(agent: Agent) = agent match {
      case _: Human => true
      case _ => false
    }

    def isZombie(agent: Agent) = agent match {
      case _: Zombie => true
      case _ => false
    }

    def human: PartialFunction[Agent, Human] = {
      case h: Human => h
    }

    def zombie: PartialFunction[Agent, Zombie] = {
      case z: Zombie => z
    }

    def zombify(zombie: Zombie, human: Human) =
      zombie.copy(
        position = human.position,
        velocity = human.velocity,
        speed = zombie.speed.copy(stamina = 0)
      )

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
            case Some(f: Floor) =>
              randomElement(f.wallSlope, rng) match {
                case Some(s) =>  Some(sum(velocity, normalize((s.x, s.y), s.intensity * speed)))
                case None => Some(velocity)
              }

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

    def rescue(world: World, agents: Vector[Agent]) = {
      val rescued = ListBuffer[Human]()
      val newAgents = ListBuffer[Agent]()

      for {
        a <- agents
      } a match {
        case h: Human =>
          val (x, y) = positionToLocation(h.position, world.side, world.side)
          if(h.rescue.informed && h.rescue.alerted && World.isRescueCell(world, x, y)) rescued += h else newAgents += h
        case a => newAgents += a
      }

      (newAgents.toVector, rescued.toVector)
    }


    def metabolism(a: Agent) =
      a match  {
        case human: Human => Human.metabolism(human)
        case zombie: Zombie => Zombie.metabolism(zombie)
        case a => a
      }

    def inform(a: Agent, neighbors: Array[Agent], rng: Random) =
      a match {
        case human: Human if !human.rescue.informed =>
          val informedNeighbors = neighbors.collect(Agent.human).count(_.rescue.informed)
          if(rng.nextDouble() < human.rescue.perceiveInformation * informedNeighbors) human.copy(rescue = human.rescue.copy(informed = true)) else human
        case a => a
      }

    def alert(a: Agent, neighbors: Array[Agent], rng: Random) =
      a match {
        case h: Human if neighbors.exists(Agent.isZombie) => Human.alerted(h)
        case a => a
      }

    def run(a: Agent, neighbors: Array[Agent]) =
      a match {
        case h: Human if neighbors.exists(Agent.isZombie) => Human.run(h)
        case z: Zombie if neighbors.exists(Agent.isHuman) => Zombie.run(z)
        case a => a
      }

    def changeDirection(world: World, index: Index[Agent], agent: Agent, granularity: Int, neighbors: Array[Agent], rng: Random) = {

      def fleeZombies(h: Human, nz: Array[Zombie], rng: Random) = {
        val pv = projectedVelocities(granularity, h.maxRotation, h.velocity, Speed.effectiveSpeed(h.speed)).filter(pv => !towardsWall(world, h.position, pv))
        if (!pv.isEmpty) {
          val nv = rng.shuffle(pv)
          h.copy(velocity = nv.maxBy { v => nz.map(n => distance(position(n), sum(h.position, v))).min })
        } else h
      }

      def pursueHuman(z: Zombie, nh: Array[Human], rng: Random) = {
        val pv = projectedVelocities(granularity, z.maxRotation, z.velocity, Speed.effectiveSpeed(z.speed))
        val nv = rng.shuffle(pv.filter(pv => !towardsWall(world, z.position, pv)))
        if (nv.isEmpty) z else z.copy(velocity = nv.minBy { v => nh.map(n => distance(position(n), sum(z.position, v))).min })
      }

      def runningHumans(agents: Array[Agent]) =
        agents.collect(Agent.human).filter { _.speed.run }

      def towardsRescue(h: Human, rng: Random) = {
        val (x, y) = location(h, world.side)
        world.cells(x)(y) match {
          case f: Floor =>
            randomElement(f.rescueSlope, rng) match {
              case Some(s) => h.copy(velocity = normalize((s.x, s.y), Speed.effectiveSpeed(h.speed)))
              case None => h
            }
          case _ => followRunning(h, rng)
        }
      }

      def followRunning(h: Human, rng: Random) = {
        if(h.followRunningProbability > 0.0) {
          val runningNeighbors = runningHumans(neighbors)
          if (!runningNeighbors.isEmpty && rng.nextDouble() < h.followRunningProbability) Human.run(h.copy(velocity = average(runningNeighbors.map(_.velocity))))
          else h
        } else h
      }


      agent match {
        case h: Human =>
          neighbors.collect(Agent.zombie) match {
            case nz if !nz.isEmpty => fleeZombies(h, nz, rng)
            case _ if h.rescue.informed && h.rescue.alerted => towardsRescue(h, rng)
            case _ => followRunning(h, rng)
          }
        case z: Zombie =>
          neighbors.collect(Agent.human) match {
            case nh if !nh.isEmpty => pursueHuman(z, nh, rng)
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
    def random(world: World, walkSpeed: Double, runSpeed: Double, maxStamina: Int, vision: Double, maxRotation: Double, followRunningProbability: Double, rescue: Rescue, rng: Random) = {
      val p = Agent.randomPosition(world, rng)
      val v = Agent.randomVelocity(walkSpeed, rng)
      Human(p, v, Speed(walkSpeed, runSpeed, maxStamina, maxStamina, false), vision, maxRotation, followRunningProbability, rescue = rescue)
    }

    def run(h: Human) =
      if(Speed.canRun(h.speed)) h.copy(velocity = normalize(h.velocity, h.speed.runSpeed), speed = h.speed.copy(run = true))
      else h

    def towardsRescue(h: Human) =
      if(h.rescue.informed) h.copy(rescue = h.rescue.copy(alerted = true)) else h

    def alerted(h: Human) = towardsRescue(run(h))

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
