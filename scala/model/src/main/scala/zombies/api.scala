package zombies

import zombies.agent.Agent
import zombies.agent.Agent.EntranceLaw
import zombies.simulation.{ArmyOption, NoArmy, NoRedCross, RedCrossOption }
import zombies.space.{Location, Position}
import zombies.world.World

import scala.util.Random

/*
 * Copyright (C) 2019 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

trait DSL {

  type Location = zombies.space.Location

  type World = zombies.world.World
  type Agent = zombies.agent.Agent
  val Agent = zombies.agent.Agent

  type Simulation = zombies.simulation.Simulation
  val Simulation = zombies.simulation.Simulation

  def physic = zombies.simulation.physic

  def stadium = simulation.environment.stadium
  def jaude = simulation.environment.jaude
  def quarantine = simulation.environment.quarantine
  def square = simulation.environment.square

  def zombieInvasion(
    world: World = quarantine,
    infectionRange: Double = physic.infectionRange,
    humanRunSpeed: Double = physic.humanRunSpeed,
    humanPerception: Double = physic.humanPerception,
    humanMaxRotation: Double = physic.humanMaxRotation,
    humanExhaustionProbability: Double = physic.humanExhaustionProbability,
    humanFollowProbability: Double = physic.humanFollowProbability,
    humanInformedRatio: Double = physic.humanInformedRatio,
    humanInformProbability: Double = physic.humanInformProbability,
    humanFightBackProbability: Double = physic.humanFightBackProbability,
    humanEntranceLaw: EntranceLaw = EntranceLaw.humanPoison(physic.entranceLambda),
    humans: Int = 250,
    zombieRunSpeed: Double = physic.zombieRunSpeed,
    zombiePerception: Double = physic.zombiePerception,
    zombieMaxRotation: Double = physic.zombieMaxRotation,
    zombiePheromoneEvaporation: Double = physic.zombiePheromoneEvaporation,
    zombieCanLeave: Boolean = physic.zombieCanLeave,
    zombies: Int = 4,
    walkSpeed: Double = physic.walkSpeed,
    rotationGranularity: Int = 5,
    army: ArmyOption = NoArmy,
    redCross: RedCrossOption = NoRedCross,
    agents: Seq[AgentGenerator] = Seq(),
    steps: Int = 500,
    random: scala.util.Random) = {

    val state = initialize(
      world = world,
      infectionRange = infectionRange,
      humanRunSpeed = humanRunSpeed,
      humanPerception = humanPerception,
      humanMaxRotation = humanMaxRotation,
      humanExhaustionProbability = humanExhaustionProbability,
      humanFollowProbability = humanFollowProbability,
      humanInformedRatio = humanInformedRatio,
      humanInformProbability = humanInformProbability,
      humanFightBackProbability = humanFightBackProbability,
      entranceLaw = humanEntranceLaw,
      humans = humans,
      zombieRunSpeed = zombieRunSpeed,
      zombiePerception = zombiePerception,
      zombieMaxRotation = zombieMaxRotation,
      zombiePheromoneEvaporation = zombiePheromoneEvaporation,
      zombieCanLeave = zombieCanLeave,
      zombies = zombies,
      walkSpeed = walkSpeed,
      rotationGranularity = rotationGranularity,
      army = army,
      redCross = redCross,
      agents = agents,
      random = random)

    Simulation.simulate(state, random, steps)
  }

  def initialize(
    world: World = quarantine,
    infectionRange: Double = physic.infectionRange,
    humanRunSpeed: Double = physic.humanRunSpeed,
    humanPerception: Double = physic.humanPerception,
    humanMaxRotation: Double = physic.humanMaxRotation,
    humanExhaustionProbability: Double = physic.humanExhaustionProbability,
    humanFollowProbability: Double = physic.humanFollowProbability,
    humanInformedRatio: Double = physic.humanInformedRatio,
    humanInformProbability: Double = physic.humanInformProbability,
    humanFightBackProbability: Double = physic.humanFightBackProbability,
    humans: Int = 250,
    zombieRunSpeed: Double = physic.zombieRunSpeed,
    zombiePerception: Double = physic.zombiePerception,
    zombieMaxRotation: Double = physic.zombieMaxRotation,
    zombiePheromoneEvaporation: Double = physic.zombiePheromoneEvaporation,
    zombieCanLeave: Boolean = physic.zombieCanLeave,
    zombies: Int = 4,
    entranceLaw: EntranceLaw = EntranceLaw.humanPoison(physic.entranceLambda),
    walkSpeed: Double = physic.walkSpeed,
    rotationGranularity: Int = 5,
    army: ArmyOption = NoArmy,
    redCross: RedCrossOption = NoRedCross,
    agents: Seq[AgentGenerator] = Seq(),
    random: scala.util.Random) = {

    val generatedAgents =
      agents.flatMap { a =>
        AgentGenerator.generateAgent(
          a,
          humanRunSpeed = humanRunSpeed,
          humanPerception = humanPerception,
          humanMaxRotation = humanMaxRotation,
          humanExhaustionProbability = humanExhaustionProbability,
          humanFollowProbability = humanFollowProbability,
          humanInformedRatio = humanInformedRatio,
          humanInformProbability = humanInformProbability,
          humanFightBackProbability = humanFightBackProbability,
          zombieRunSpeed = zombieRunSpeed,
          zombiePerception = zombiePerception,
          zombieMaxRotation = zombieMaxRotation,
          zombieCanLeave = zombieCanLeave,
          world = world,
          walkSpeed = walkSpeed,
          random = random
        )
      }

    Simulation.initialize(
      world = world,
      infectionRange = infectionRange,
      humanRunSpeed = humanRunSpeed,
      humanPerception = humanPerception,
      humanMaxRotation = humanMaxRotation,
      humanExhaustionProbability = humanExhaustionProbability,
      humanFollowProbability = humanFollowProbability,
      humanInformedRatio = humanInformedRatio,
      humanInformProbability = humanInformProbability,
      humanFightBackProbability = humanFightBackProbability,
      entranceLaw = entranceLaw,
      humans = humans,
      zombieRunSpeed = zombieRunSpeed,
      zombiePerception = zombiePerception,
      zombieMaxRotation = zombieMaxRotation,
      zombiePheromoneEvaporation = zombiePheromoneEvaporation,
      zombieCanLeave = zombieCanLeave,
      zombies = zombies,
      walkSpeed = walkSpeed,
      rotationGranularity = rotationGranularity,
      army = army,
      redCross = redCross,
      agents = generatedAgents,
      random = random)
  }

  type Army = simulation.Army
  type RedCross = simulation.RedCross

  def Army(
    size: Int,
    fightBackProbability: Double = 1.0,
    exhaustionProbability: Double = physic.humanExhaustionProbability,
    perception: Double = physic.humanPerception,
    runSpeed: Double = physic.humanRunSpeed,
    followProbability: Double = physic.humanFollowProbability,
    maxRotation: Double = physic.humanMaxRotation,
    informProbability: Double = 0.0,
    aggressive: Boolean = true) =
    simulation.Army(
      size,
      fightBackProbability = fightBackProbability,
      exhaustionProbability = exhaustionProbability,
      perception = perception,
      runSpeed = runSpeed,
      followProbability = followProbability,
      maxRotation = maxRotation,
      informProbability = informProbability,
      aggressive = aggressive
    )


  def RedCross(
    size: Int,
    exhaustionProbability: Option[Double] = None,
    followProbability: Double = 0.0,
    informProbability: Double = physic.humanInformProbability,
    aggressive: Boolean = true,
    activationDelay: Int = 10,
    efficiencyProbability: Double = 1.0) =
    simulation.RedCross(
      size,
      exhaustionProbability = exhaustionProbability,
      followProbability = followProbability,
      informProbability = informProbability,
      aggressive = aggressive,
      activationDelay = activationDelay,
      efficiencyProbability = efficiencyProbability
    )

  def World(s: String) = zombies.world.World.parse()(s)

  object AgentGenerator {

    object Optional {
      implicit def toOptional[T](t: T) = Value(t)

      implicit def toOption[T](optional: Optional[T]) =
        optional match {
          case Value(v) => Some(v)
          case NoValue => None
        }

      implicit def fromOption[T](option: Option[T]) =
        option match {
          case Some(v) => Value(v)
          case None => NoValue
        }
    }

    sealed trait Optional[+T] {
      def toOption = Optional.toOption(this)
    }
    case class Value[T](v: T) extends Optional[T]
    case object NoValue extends Optional[Nothing]

    def generateAgent(
      generator: AgentGenerator,
      walkSpeed: Double,
      humanRunSpeed: Double,
      humanPerception: Double,
      humanMaxRotation: Double,
      humanExhaustionProbability: Double,
      humanFollowProbability: Double,
      humanInformedRatio: Double,
      humanInformProbability: Double,
      humanFightBackProbability: Double,
      zombieRunSpeed: Double,
      zombiePerception: Double,
      zombieMaxRotation: Double,
      zombieCanLeave: Boolean,
      world: World,
      random: Random) = {

      def toPosition(l: Location) = {
        val (x, y) = l
        zombies.world.World.get(world, x, y) match {
          case Some(_: zombies.world.Floor) => Some(zombies.world.World.cellCenter(world, (x, y)))
          case _ => None
        }
      }

      def generateHuman(human: Human) = {
        val informed = human.informed.getOrElse(random.nextDouble() < humanInformedRatio)
        val rescue = zombies.agent.Rescue(informed = informed, informProbability = human.informProbability.getOrElse(humanInformProbability))

        zombies.agent.Human(
          world = world,
          walkSpeedParameter = human.walkSpeed.getOrElse(walkSpeed),
          runSpeedParameter = human.runSpeed.getOrElse(humanRunSpeed),
          exhaustionProbability = human.exhaustionProbability.getOrElse(humanExhaustionProbability),
          perceptionParameter = human.perception.getOrElse(humanPerception),
          maxRotation = human.maxRotation.getOrElse(humanMaxRotation),
          followRunningProbability = human.followProbability.getOrElse(humanFollowProbability),
          fight = zombies.agent.Fight(human.fightBackProbability.getOrElse(humanFightBackProbability), aggressive = human.aggressive),
          rescue = rescue,
          canLeave = true,
          rng = random)
      }

      def generateZombie(zombie: Zombie) = {
        zombies.agent.Zombie(
          world = world,
          walkSpeedParameter = zombie.walkSpeed.getOrElse(walkSpeed),
          runSpeedParameter = zombie.runSpeed.getOrElse(zombieRunSpeed),
          perceptionParameter = zombie.perception.getOrElse(zombiePerception),
          maxRotation = zombie.maxRotation.getOrElse(zombieMaxRotation),
          canLeave = zombieCanLeave,
          random = random
        )
      }

      generator match {
        case human: Human =>
          human.location.toOption match {
            case Some(l) => toPosition(l).map(l => generateHuman(human).copy(position = l))
            case None => Some(generateHuman(human))
          }
        case zombie: Zombie =>
          zombie.location.toOption match {
            case Some(l) => toPosition(l).map(l => generateZombie(zombie).copy(position = l))
            case None => Some(generateZombie(zombie))
          }

      }
    }



  }

  sealed trait AgentGenerator

  case class Human(
    walkSpeed: AgentGenerator.Optional[Double] = None,
    runSpeed:  AgentGenerator.Optional[Double] = None,
    exhaustionProbability:  AgentGenerator.Optional[Double] = None,
    perception:  AgentGenerator.Optional[Double] = None,
    maxRotation:  AgentGenerator.Optional[Double] = None,
    followProbability:  AgentGenerator.Optional[Double] = None,
    location:  AgentGenerator.Optional[Location] = None,
    fightBackProbability:  AgentGenerator.Optional[Double] = None,
    informProbability:  AgentGenerator.Optional[Double] = None,
    aggressive: Boolean = false,
    informed:  AgentGenerator.Optional[Boolean] = None) extends AgentGenerator

  case class Zombie(
    walkSpeed: AgentGenerator.Optional[Double] = None,
    runSpeed:  AgentGenerator.Optional[Double] = None,
    perception:  AgentGenerator.Optional[Double] = None,
    maxRotation:  AgentGenerator.Optional[Double] = None,
    location:  AgentGenerator.Optional[Location] = None) extends AgentGenerator

  implicit class EntranceLawParameterDecorator(e: EntranceLaw.Parameter) {
    def around(range: Double) = {
      val cellSide = space.cellSide(e.world.side)
      Agent.around(_root_.zombies.world.World.cellCenter(e.world, e.entranceLocation), range * cellSide, e.index, e.neighborhoodCache).toVector
    }

    def enter(generator: Iterable[AgentGenerator]) = generator.flatMap { generator =>
      AgentGenerator.generateAgent(
        generator,
        walkSpeed = e.simulation.walkSpeedParameter,
        humanRunSpeed = e.simulation.humanRunSpeedParameter,
        humanPerception = e.simulation.humanPerceptionParameter,
        humanMaxRotation = e.simulation.humanMaxRotation,
        humanExhaustionProbability = e.simulation.humanExhaustionProbability,
        humanFollowProbability = e.simulation.humanFollowProbability,
        humanInformedRatio = e.simulation.humanInformedRatio,
        humanInformProbability = e.simulation.humanInformProbability,
        humanFightBackProbability = e.simulation.humanFightBackProbability,
        zombieRunSpeed = e.simulation.zombieRunSpeedParameter,
        zombiePerception = e.simulation.zombiePerceptionParameter,
        zombieMaxRotation = e.simulation.zombieMaxRotation,
        zombieCanLeave = e.simulation.zombieCanLeave,
        world = e.world,
        random = e.random
      )
    }.toVector
  }

  val CaptureTrap = world.CaptureTrap
  val DeathTrap = world.DeathTrap

  implicit class TrapDecorator(w: World) {
    def withTrap(t: (Location, world.Trap)*) = world.World.setTraps(w, t)
  }



}

object api extends DSL