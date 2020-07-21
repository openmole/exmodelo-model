package zombies

import agent._
import world._
import zombies.agent.Agent.EntranceLaw

import scala.util.Random

package simulation {

  object Event {
    def rescued : PartialFunction[Event, Rescued] = {
      case e: Rescued => e
    }

    def zombified: PartialFunction[Event, Zombified] = {
      case e: Zombified => e
    }

    def killed: PartialFunction[Event, Killed] = {
      case e: Killed => e
    }

    def gone: PartialFunction[Event, Gone] = {
      case e: Gone => e
    }

    def humanGone: PartialFunction[Event, Gone] = {
      case g@Gone(_: Human) => g
    }

    def zombieGone: PartialFunction[Event, Gone] = {
      case g@Gone(_: Zombie) => g
    }

    def flee: PartialFunction[Event, FleeZombie] = {
      case e: FleeZombie => e
    }

    def pursue: PartialFunction[Event, PursueHuman] = {
      case e: PursueHuman => e
    }

    def trapped: PartialFunction[Event, Trapped] = {
      case e: Trapped => e
    }
  }

  sealed trait Event
  case class Zombified(human: Human) extends Event
  case class Killed(zombie: Zombie) extends Event
  case class Rescued(human: Human) extends Event
  case class Gone(agent: Agent) extends Event
  case class FleeZombie(human: Human) extends Event
  case class PursueHuman(zombie: Zombie) extends Event
  case class Trapped(zombie: Zombie) extends Event

  sealed trait ArmyOption
  case object NoArmy extends ArmyOption
  case class Army(
    size: Int,
    fightBackProbability: Double = 1.0,
    exhaustionProbability: Double = physic.humanExhaustionProbability,
    perception: Double = physic.humanPerception,
    runSpeed: Double = physic.humanRunSpeed,
    followProbability: Double = physic.humanFollowProbability,
    maxRotation: Double = physic.humanMaxRotation,
    informProbability: Double = 0.0,
    aggressive: Boolean = true) extends ArmyOption

  sealed trait RedCrossOption
  case object NoRedCross extends RedCrossOption
  case class RedCross(
    size: Int,
    exhaustionProbability: Option[Double] = None,
    followProbability: Double = 0.0,
    informProbability: Double = physic.humanInformProbability,
    aggressive: Boolean = true,
    activationDelay: Int,
    efficiencyProbability: Double) extends RedCrossOption


  case class HummanParameter()

  object Simulation {

    def initialize(
      world: World,
      infectionRange: Double = physic.infectionRange,
      humanRunSpeed: Double = physic.humanRunSpeed,
      humanPerception: Double = physic.humanPerception,
      humanMaxRotation: Double = physic.humanMaxRotation,
      humanExhaustionProbability: Double = physic.humanExhaustionProbability,
      humanFollowProbability: Double = physic.humanFollowProbability,
      humanInformedRatio: Double = physic.humanInformedRatio,
      humanInformProbability: Double = physic.humanInformProbability,
      humanFightBackProbability: Double = physic.humanFightBackProbability,
      humans: Int,
      zombieRunSpeed: Double = physic.zombieRunSpeed,
      zombiePerception: Double = physic.zombiePerception,
      zombieMaxRotation: Double = physic.zombieMaxRotation,
      zombiePheromoneEvaporation: Double = physic.zombiePheromoneEvaporation,
      zombieCanLeave: Boolean = physic.zombieCanLeave,
      zombies: Int,
      walkSpeed: Double = physic.walkSpeed,
      entranceLaw: EntranceLaw = EntranceLaw.humanPoison(physic.entranceLambda),
      rotationGranularity: Int = 5,
      army: ArmyOption = NoArmy,
      redCross: RedCrossOption = NoRedCross,
      agents: Seq[Agent] = Seq(),
      random: Random) = {

      //val cellSide = space.cellSide(world.side)

      def generateHuman = {
        val informed = random.nextDouble() < humanInformedRatio
        val rescue = Rescue(informed = informed, informProbability = humanInformProbability)
        Human(
          world = world,
          walkSpeedParameter = walkSpeed,
          runSpeedParameter = humanRunSpeed,
          exhaustionProbability = humanExhaustionProbability,
          perceptionParameter = humanPerception,
          maxRotation = humanMaxRotation,
          followRunningProbability = humanFollowProbability,
          fight = Fight(humanFightBackProbability),
          rescue = rescue,
          canLeave = true,
          function = Human.Civilian,
          rng = random)
      }

      def generateZombie =
        Zombie(
          world = world,
          walkSpeedParameter = walkSpeed,
          runSpeedParameter = zombieRunSpeed,
          perceptionParameter = zombiePerception,
          maxRotation = zombieMaxRotation,
          canLeave = zombieCanLeave,
          random = random)


      def generateSoldier(army: Army) = {
        val rescue = Rescue(informed = true, alerted = true, informProbability = army.informProbability)
        Human(
          world = world,
          walkSpeedParameter = walkSpeed,
          runSpeedParameter = army.runSpeed,
          exhaustionProbability = army.exhaustionProbability,
          perceptionParameter = army.perception,
          maxRotation = army.maxRotation,
          followRunningProbability = army.followProbability,
          fight = Fight(army.fightBackProbability, aggressive = army.aggressive),
          rescue = rescue,
          canLeave = false,
          function = Human.Army,
          rng = random)
      }

      def soldiers =
        army match {
          case NoArmy => Vector.empty
          case a: Army => Vector.fill(a.size)(generateSoldier(a))
        }

      def generateRedCrossVolunteers(redCross: RedCross) = {
        val rescue = Rescue(informProbability = redCross.informProbability, noFollow = true)
        val antidote = Antidote(activationDelay = redCross.activationDelay, efficiencyProbability = redCross.efficiencyProbability, exhaustionProbability = redCross.exhaustionProbability)
        Human(
          world = world,
          walkSpeedParameter = walkSpeed,
          runSpeedParameter = humanRunSpeed,
          exhaustionProbability = humanExhaustionProbability,
          perceptionParameter = humanPerception,
          maxRotation = humanMaxRotation,
          followRunningProbability = redCross.followProbability,
          fight = Fight(humanFightBackProbability, aggressive = redCross.aggressive),
          rescue = rescue,
          canLeave = false,
          antidote = antidote,
          function = Human.RedCross,
          rng = random)
      }

      def redCrossVolunteers =
        redCross match {
          case NoRedCross => Vector.empty
          case a: RedCross => Vector.fill(a.size)(generateRedCrossVolunteers(a))
        }

      val allAgents =
        Vector.fill(humans)(generateHuman) ++
          Vector.fill(zombies)(generateZombie) ++
          soldiers ++
          redCrossVolunteers ++
          agents

      Simulation(
        world = world,
        agents = allAgents,
        infectionRangeParameter = infectionRange,
        humanRunSpeedParameter = humanRunSpeed,
        humanPerceptionParameter = humanPerception,
        humanMaxRotation = humanMaxRotation,
        humanExhaustionProbability = humanExhaustionProbability,
        humanFollowProbability = humanFollowProbability,
        humanFightBackProbability = humanFightBackProbability,
        humanInformedRatio = humanInformedRatio,
        humanInformProbability = humanInformProbability,
        zombieRunSpeedParameter = zombieRunSpeed,
        zombiePerceptionParameter = zombiePerception,
        zombieMaxRotation = zombieMaxRotation,
        zombieCanLeave = zombieCanLeave,
        walkSpeedParameter = walkSpeed,
        zombiePheromone = Pheromone(zombiePheromoneEvaporation),
        rotationGranularity = rotationGranularity,
        entranceLaw = entranceLaw
      )

    }

    def step(step: Int, simulation: Simulation, neighborhoodCache: NeighborhoodCache, rng: Random) = {
      val index = Agent.index(simulation.agents, simulation.world.side)
      val w1 = Agent.releasePheromone(index, simulation.world, simulation.zombiePheromone)
      val (ai, infected, died) = Agent.fight(w1, index, simulation.agents, simulation.infectionRange, Agent.zombify(_, _), rng)

      val (na1, moveEvents) =
        ai.map { a0 =>
          val ns = Agent.neighbors(index, a0, Agent.perception(a0), neighborhoodCache)

          val evolve =
            Agent.inform(ns, w1, rng) _ andThen
              Agent.alert(ns, rng) _ andThen
              Agent.takeAntidote _ andThen
              Agent.chooseRescue _ andThen
              Agent.run(ns) _ andThen
              Agent.metabolism(rng) _

          val a1 = evolve(a0)

          val (a2, ev) = Agent.changeDirection(w1, simulation.rotationGranularity, ns, rng)(a1)

          Agent.move(w1, simulation.rotationGranularity, rng) (a2) match {
            case Some(a) => (Some(a), ev.toVector)
            case None => (None, Vector(Gone(a2)) ++ ev)
          }
        }.unzip

      val (na2, rescued) = Agent.rescue(w1, na1.flatten)

      val newAgents = Agent.joining(w1, index, neighborhoodCache, simulation, na2, rng)

      val events =
        infected.map(i => Zombified(i)) ++
          died.map(d => Killed(d)) ++
          rescued.map(r => Rescued(r)) ++
          moveEvents.flatten

      (simulation.copy(agents = na2 ++ newAgents, world = w1), events)
    }

    type SimulationResult = (List[Simulation], List[Vector[Event]])


    def simulate(simulation: Simulation, rng: Random, steps: Int): SimulationResult = {
      def result(s: Simulation, events: Vector[Event], acc: SimulationResult) = (s :: acc._1, events :: acc._2)

      val (simulations, events) = simulate(simulation, rng, steps, result, (List(), List()))
      (simulations.reverse, events.reverse)
    }

    def simulate[ACC](simulation: Simulation, rng: Random, steps: Int, accumulate: (Simulation, Vector[Event], ACC) => ACC, accumulator: ACC): ACC = {
      val neighborhoodCache = World.visibleNeighborhoodCache(simulation.world, math.max(simulation.humanPerception, simulation.zombiePerception))

      def run0(s: Int, simulation: Simulation, events: Vector[Event], r: (Simulation, Vector[Event], ACC) => ACC, accumulator: ACC): ACC =
        if (s == 0) r(simulation, events, accumulator)
        else {
          val newAccumulator = r(simulation, events, accumulator)
          val (newSimulation, newEvents) = step(steps - s, simulation, neighborhoodCache, rng)
          run0(s - 1, newSimulation, newEvents, r, newAccumulator)
        }

      run0(steps, simulation, Vector.empty, accumulate, accumulator)
    }


  }

  case class Simulation(
    world: World,
    agents: Vector[Agent],
    infectionRangeParameter: Double,
    humanRunSpeedParameter: Double,
    humanPerceptionParameter: Double,
    humanMaxRotation: Double,
    humanExhaustionProbability: Double,
    humanFollowProbability: Double,
    humanFightBackProbability: Double,
    humanInformedRatio: Double,
    humanInformProbability: Double,
    zombieRunSpeedParameter: Double,
    zombiePerceptionParameter: Double,
    zombieMaxRotation: Double,
    zombieCanLeave: Boolean,
    walkSpeedParameter: Double,
    zombiePheromone: PheromoneMechanism,
    rotationGranularity: Int,
    entranceLaw: EntranceLaw) {
      val cellSide = space.cellSide(world.side)
      val infectionRange = infectionRangeParameter * cellSide
      val humanRunSpeed = humanRunSpeedParameter * cellSide
      val humanPerception = humanPerceptionParameter * cellSide
      val zombieRunSpeed = zombieRunSpeedParameter * cellSide
      val zombiePerception = zombiePerceptionParameter * cellSide
      val walkSpeed = walkSpeedParameter * cellSide
  }


  object environment {
    def all = Vector(stadium, jaude)
    def stadium = world.World.stadium(15, 15, 5)
    def jaude = world.World.jaude
    def quarantine = world.World.quarantineStadium(15, 15)
    def square = world.World.square(15)
  }

  object physic {
    /* General parameters */
    val walkSpeed = 0.1
    val infectionRange = 0.32

    /* Human parameters */
    val humanPerception = 1.4
    val humanRunSpeed = 0.49
    val humanExhaustionProbability = 0.45
    val humanMaxRotation = 60
    val humanInformedRatio = 0.11
    val humanInformProbability = 0.09
    val humanFollowProbability = 0.27
    val humanFightBackProbability = 0.01

    /* Zombie parameters */
    val zombiePerception = 2.9
    val zombieRunSpeed = 0.28
    val zombiePheromoneEvaporation = 0.38
    val zombieMaxRotation = 30
    val zombieCanLeave = true

    /* Additional parameters */
    val entranceLambda = 0.1
  }

}
