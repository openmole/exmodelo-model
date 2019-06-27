package zombies

import zombies.agent.Agent
import zombies.simulation.{ArmyOption, NoArmy, NoRedCross, RedCrossOption, Simulation}
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

  def physic = zombies.simulation.physic

  def stadium = simulation.environment.stadium
  def jaude = simulation.environment.jaude
  def quarantine = simulation.environment.quarantine
  def square = simulation.environment.square

  def zombieInvasion(
    world: World = quarantine,
    infectionRange: Double = physic.infectionRange,
    demographicEntrances: Boolean = false,
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
      demographicEntrances = demographicEntrances,
      humanRunSpeed = humanRunSpeed,
      humanPerception = humanPerception,
      humanMaxRotation = humanMaxRotation,
      humanExhaustionProbability = humanExhaustionProbability,
      humanFollowProbability = humanFollowProbability,
      humanInformedRatio = humanInformedRatio,
      humanInformProbability = humanInformProbability,
      humanFightBackProbability = humanFightBackProbability,
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

    simulation.simulate(state, random, steps)
  }

  def initialize(
    world: World = quarantine,
    infectionRange: Double = physic.infectionRange,
    demographicEntrances: Boolean = false,
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
    walkSpeed: Double = physic.walkSpeed,
    rotationGranularity: Int = 5,
    army: ArmyOption = NoArmy,
    redCross: RedCrossOption = NoRedCross,
    agents: Seq[AgentGenerator] = Seq(),
    steps: Int = 500,
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
      demographicEntrances = demographicEntrances,
      humanRunSpeed = humanRunSpeed,
      humanPerception = humanPerception,
      humanMaxRotation = humanMaxRotation,
      humanExhaustionProbability = humanExhaustionProbability,
      humanFollowProbability = humanFollowProbability,
      humanInformedRatio = humanInformedRatio,
      humanInformProbability = humanInformProbability,
      humanFightBackProbability = humanFightBackProbability,
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
      fightBackProbability = 1.0,
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


      val cellSide = space.cellSide(world.side)

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
          walkSpeed = human.walkSpeed.getOrElse(walkSpeed) * cellSide,
          runSpeed = human.runSpeed.getOrElse(humanRunSpeed) * cellSide,
          exhaustionProbability = human.exhaustionProbability.getOrElse(humanExhaustionProbability),
          perception = human.perception.getOrElse(humanPerception) * cellSide,
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
          walkSpeed = zombie.walkSpeed.getOrElse(walkSpeed) * cellSide,
          runSpeed = zombie.runSpeed.getOrElse(zombieRunSpeed) * cellSide,
          perception = zombie.perception.getOrElse(zombiePerception) * cellSide,
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

  val CaptureTrap = world.CaptureTrap
  val DeathTrap = world.DeathTrap

  implicit class TrapDecorator(w: World) {
    def withTrap(t: (Location, world.Trap)*) = world.World.setTraps(w, t)
  }

}

object api extends DSL