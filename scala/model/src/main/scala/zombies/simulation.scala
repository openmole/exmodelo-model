package zombies

import agent.*
import world.*
import zombies.agent.Agent.EntranceLaw

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

object simulation:

  object Event:
    def rescued : PartialFunction[Event, Rescued] =
      case e: Rescued => e

    def zombified: PartialFunction[Event, Zombified] =
      case e: Zombified => e

    def killed: PartialFunction[Event, Killed] =
      case e: Killed => e

    def gone: PartialFunction[Event, Gone] =
      case e: Gone => e

    def humanGone: PartialFunction[Event, Gone] =
      case g@Gone(_: Human) => g

    def zombieGone: PartialFunction[Event, Gone] =
      case g@Gone(_: Zombie) => g

    def flee: PartialFunction[Event, FleeZombie] =
      case e: FleeZombie => e

    def pursue: PartialFunction[Event, PursueHuman] =
      case e: PursueHuman => e

    def trapped: PartialFunction[Event, Trapped] =
      case e: Trapped => e

    def antidoteActivated: PartialFunction[Event, AntidoteActivated] =
      case e: AntidoteActivated => e

    def immunityLoss: PartialFunction[Event, ImmunityLoss] =
      case e: ImmunityLoss => e

  sealed trait Event
  case class Zombified(human: Human) extends Event
  case class Killed(zombie: Zombie) extends Event
  case class Rescued(human: Human) extends Event
  case class Gone(agent: Agent) extends Event
  case class FleeZombie(human: Human) extends Event
  case class PursueHuman(zombie: Zombie) extends Event
  case class Trapped(zombie: Zombie) extends Event
  case class AntidoteActivated(human: Human) extends Event
  case class ImmunityLoss(human: Human) extends Event

  sealed trait ArmyOption
  case object NoArmy extends ArmyOption
  case class Army(
    size: Int = 1,
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
    size: Int = 1,
    vaccinatedExhaustionProbability: Option[Double] = None,
    followProbability: Double = 0.0,
    informProbability: Double = physic.humanInformProbability,
    aggressive: Boolean = true,
    activationDelay: Int,
    efficiencyProbability: Double,
    immunityLossProbability: Double = 0.0,
    maxRotation: Double = physic.humanMaxRotation,
    fightBackProbability: Double = physic.humanFightBackProbability,
    exhaustionProbability:  Double = physic.humanExhaustionProbability,
    perception: Double = physic.humanPerception,
    runSpeed: Double = physic.humanRunSpeed,
    informedRatio: Double = physic.humanInformedRatio,
    alertedRatio: Double = 0.0) extends RedCrossOption


  case class HummanParameter()

  object Simulation:

    def step(step: Int, simulation: Simulation, worldCache: WorldCache, rng: Random) =
      val index = Agent.index(simulation.agents, simulation.world.side)
      val w1 = Agent.releasePheromone(index, simulation.world, simulation.zombiePheromone)
      val (ai, infected, died) = Agent.fight(w1, index, simulation.agents, simulation.relativeInfectionRange, Agent.zombify(_, _), rng)

      val (na1, moveEvents) =
        ai.map { a0 =>
          val ns = Agent.neighbors(index, a0, Agent.perception(a0), worldCache)

          val evolve =
            Agent.inform(ns, w1, rng) _ andThen
              Agent.alert(ns, rng) _ andThen
              Agent.takeAntidote _ andThen
              Agent.looseImmunity(rng) andThen
              Agent.getAntidote(ns, rng) _ andThen
              Agent.chooseRescue _ andThen
              Agent.run(ns) _ andThen
              Agent.metabolism(rng) _

          val a1 = evolve(a0)

          val (a2, ev1) = Agent.changeDirection(w1, simulation.rotationGranularity, ns, rng)(a1)
          val ev = ev1 ++ Agent.observedEvents(a0, a2)

          Agent.move(w1, simulation.rotationGranularity, rng) (a2) match {
            case Some(a) => (Some(a), ev.toVector)
            case None => (None, Vector(Gone(a2)) ++ ev)
          }
        }.unzip

      val (na2, rescued) = Agent.rescue(w1, na1.flatten)

      val newAgents = Agent.joining(w1, index, worldCache, simulation, step, na2, rng)

      val events =
        infected.map(i => Zombified(i)) ++
          died.map(d => Killed(d)) ++
          rescued.map(r => Rescued(r)) ++
          moveEvents.flatten

      (simulation.copy(agents = na2 ++ newAgents, world = w1), events)


    def simulateBlind(simulation: Simulation, rng: Random, steps: Int) =
      val neighborhoodCache = WorldCache.compute(simulation.world, simulation.neighborhood, math.max(simulation.relativeHumanPerception, simulation.relativeZombiePerception))

      def run0(s: Int, simulation: Simulation): Unit =
        if s == 0
        then ()
        else
          val (newSimulation, _) = step(steps - s, simulation, neighborhoodCache, rng)
          run0(s - 1, newSimulation)

      run0(steps, simulation)

    def simulate(simulation: Simulation, rng: Random, steps: Int, cache: Option[WorldCache] = None): SimulationResult =
      val allSimulations = new collection.mutable.ArrayBuffer[Simulation](steps)
      val allEvents = new collection.mutable.ArrayBuffer[Vector[Event]](steps)

      def result(step: Int, s: Simulation, events: Vector[Event]) =
        allSimulations.append(s)
        allEvents.append(events)

      listenSimulation(simulation, rng, steps, result)
      SimulationResult(allSimulations, allEvents)

    def listenSimulation[ACC](simulation: Simulation, rng: Random, steps: Int, stepListener: (Int, Simulation, Vector[Event]) => Unit, cache: Option[WorldCache] = None): Unit =
      val worldCache =
        cache.getOrElse:
          WorldCache.compute(simulation.world, simulation.neighborhood, math.max(simulation.relativeHumanPerception, simulation.relativeZombiePerception))

      @tailrec def run0(s: Int, simulation: Simulation, events: Vector[Event], r: (Int, Simulation, Vector[Event]) => Unit): Unit =
        val currentStep = steps - s
        if s == 0
        then r(currentStep, simulation, events)
        else
          r(currentStep, simulation, events)
          val (newSimulation, newEvents) = step(currentStep, simulation, worldCache, rng)
          run0(s - 1, newSimulation, newEvents, r)

      run0(steps, simulation, Vector.empty, stepListener)


  case class Simulation(
    world: World,
    agents: Vector[Agent],
    infectionRange: Double,
    humanRunSpeed: Double,
    humanPerception: Double,
    humanMaxRotation: Double,
    humanExhaustionProbability: Double,
    humanFollowProbability: Double,
    humanFightBackProbability: Double,
    humanInformedRatio: Double,
    humanInformProbability: Double,
    zombieRunSpeed: Double,
    zombiePerception: Double,
    zombieMaxRotation: Double,
    zombieCanLeave: Boolean,
    walkSpeedParameter: Double,
    zombiePheromone: PheromoneMechanism,
    rotationGranularity: Int,
    entranceLaw: EntranceLaw,
    neighborhood: Neighborhood = Neighborhood.Visible):
      def cellSide = space.cellSide(world.side)
      def relativeInfectionRange = infectionRange * cellSide
      def relativeHumanPerception = humanPerception * cellSide
      def relativeZombiePerception = zombiePerception * cellSide

  case class SimulationResult(simulations: Iterable[Simulation], events: Iterable[Vector[Event]])

  object environment:
    def all = Vector(stadium, jaude)
    def stadium = world.World.stadium(15, 15, 5)
    def jaude = world.World.jaude
    def quarantine = world.World.quarantineStadium(15, 15)
    def square = world.World.square(15)

  object physic:
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
