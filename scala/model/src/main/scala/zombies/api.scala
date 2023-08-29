package zombies

import zombies.agent.Agent
import zombies.agent.Agent.{EntranceLaw, looseImmunity, zombie}
import zombies.simulation.{ArmyOption, NoArmy, NoRedCross, RedCrossOption}
import zombies.space.{Location, Position}
import zombies.world.{Neighborhood, World}

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

trait DSL:

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
    entrance: EntranceLaw = EntranceLaw.humanPoison(physic.entranceLambda),
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
    random: scala.util.Random) =

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
      entrance = entrance,
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
    entrance: EntranceLaw = EntranceLaw.humanPoison(physic.entranceLambda),
    walkSpeed: Double = physic.walkSpeed,
    rotationGranularity: Int = 5,
    army: ArmyOption = NoArmy,
    redCross: RedCrossOption = NoRedCross,
    agents: Seq[AgentGenerator] = Seq(),
    neighborhood: Neighborhood = Neighborhood.Visible,
    random: scala.util.Random) = 
    
    def generateHuman =
      import _root_.zombies.agent._

      val informed = random.nextDouble() < humanInformedRatio
      val rescue = Rescue(informed = informed, informProbability = humanInformProbability)
      _root_.zombies.agent.Human(
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
        function = _root_.zombies.agent.Human.Civilian,
        rng = random)

    def generateSoldier(army: Army) =
      import _root_.zombies.agent._
      val rescue = Rescue(informed = true, alerted = true, informProbability = army.informProbability)
      _root_.zombies.agent.Human(
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
        function = _root_.zombies.agent.Human.Army,
        rng = random)

    def soldiers =
      army match
        case NoArmy => Vector.empty
        case a: Army => Vector.fill(a.size)(generateSoldier(a))

    def generateZombie =
      _root_.zombies.agent.Zombie(
        world = world,
        walkSpeedParameter = walkSpeed,
        runSpeedParameter = zombieRunSpeed,
        perceptionParameter = zombiePerception,
        maxRotation = zombieMaxRotation,
        canLeave = zombieCanLeave,
        random = random)

    def generateRedCrossVolunteers(redCross: RedCross) =
      import _root_.zombies.agent._

      val informed = random.nextDouble() < redCross.informedRatio
      val alerted = random.nextDouble() < redCross.alertedRatio

      val rescue = Rescue(informProbability = redCross.informProbability, noFollow = true, alerted = alerted, informed = informed)
      val antidote = Antidote(activationDelay = redCross.activationDelay, immunityLossProbability = redCross.immunityLossProbability, efficiencyProbability = redCross.efficiencyProbability, vaccinatedExhaustionProbability = redCross.vaccinatedExhaustionProbability)
      _root_.zombies.agent.Human(
        world = world,
        walkSpeedParameter = walkSpeed,
        runSpeedParameter = redCross.runSpeed,
        exhaustionProbability = redCross.exhaustionProbability,
        perceptionParameter = redCross.perception,
        maxRotation = humanMaxRotation,
        followRunningProbability = redCross.followProbability,
        fight = Fight(redCross.fightBackProbability, aggressive = redCross.aggressive),
        rescue = rescue,
        canLeave = false,
        antidote = antidote,
        function = _root_.zombies.agent.Human.RedCross,
        rng = random)

    def redCrossVolunteers =
      redCross match
        case NoRedCross => Vector.empty
        case a: RedCross => Vector.fill(a.size)(generateRedCrossVolunteers(a))

    val generatedAgents =
      agents.flatMap: a =>
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

    val allAgents =
      Vector.fill(humans)(generateHuman) ++
        Vector.fill(zombies)(generateZombie) ++
        soldiers ++
        redCrossVolunteers ++
        generatedAgents

    Simulation(
      world = world,
      agents = allAgents,
      infectionRange = infectionRange,
      humanRunSpeed = humanRunSpeed,
      humanPerception = humanPerception,
      humanMaxRotation = humanMaxRotation,
      humanExhaustionProbability = humanExhaustionProbability,
      humanFollowProbability = humanFollowProbability,
      humanFightBackProbability = humanFightBackProbability,
      humanInformedRatio = humanInformedRatio,
      humanInformProbability = humanInformProbability,
      zombieRunSpeed = zombieRunSpeed,
      zombiePerception = zombiePerception,
      zombieMaxRotation = zombieMaxRotation,
      zombieCanLeave = zombieCanLeave,
      walkSpeedParameter = walkSpeed,
      zombiePheromone = _root_.zombies.agent.Pheromone(zombiePheromoneEvaporation),
      rotationGranularity = rotationGranularity,
      entranceLaw = entrance,
      neighborhood = neighborhood
    )

  type Army = simulation.Army
  type RedCross = simulation.RedCross

  def Army(
    size: Int = 1,
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
    size: Int = 1,
    vaccinatedExhaustionProbability: Option[Double] = None,
    followProbability: Double = 0.0,
    informProbability: Double = physic.humanInformProbability,
    aggressive: Boolean = false,
    activationDelay: Int = 10,
    efficiencyProbability: Double = 1.0,
    immunityLossProbability: Double = 0.0,
    fightBackProbability: Double = 1.0,
    exhaustionProbability: Double = physic.humanExhaustionProbability,
    perception: Double = physic.humanPerception,
    runSpeed: Double = physic.humanRunSpeed,
    maxRotation: Double = physic.humanMaxRotation,
    informedRatio: Double = 0.0,
    alertedRatio: Double = 0.0) =
    simulation.RedCross(
      size,
      vaccinatedExhaustionProbability = vaccinatedExhaustionProbability,
      followProbability = followProbability,
      informProbability = informProbability,
      aggressive = aggressive,
      activationDelay = activationDelay,
      efficiencyProbability = efficiencyProbability,
      immunityLossProbability = immunityLossProbability,
      fightBackProbability = fightBackProbability,
      exhaustionProbability = exhaustionProbability,
      perception = perception,
      runSpeed = runSpeed,
      maxRotation = maxRotation,
      informedRatio = informedRatio,
      alertedRatio = alertedRatio
    )

  def World(s: String) = zombies.world.World.parse()(s)

  object AgentGenerator:

    object Optional:
      implicit def toOptional[T](t: T): Value[T] = Value(t)

      implicit def toOption[T](optional: Optional[T]): Option[T] =
        optional match
          case Value(v) => Some(v)
          case NoValue => None

      given [T]: Conversion[Option[T], Optional[T]] =
        case Some(v) => Value(v)
        case None => NoValue

    sealed trait Optional[+T]:
      def toOption = Optional.toOption(this)

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

      def generateSoldier(soldier: Soldier) = {
        import _root_.zombies.agent._
        val rescue = Rescue(informed = true, alerted = true, informProbability = soldier.informProbability)
        _root_.zombies.agent.Human(
          world = world,
          walkSpeedParameter = walkSpeed,
          runSpeedParameter = soldier.runSpeed.getOrElse(humanRunSpeed),
          exhaustionProbability = soldier.exhaustionProbability.getOrElse(humanExhaustionProbability),
          perceptionParameter = soldier.perception.getOrElse(humanPerception),
          maxRotation = soldier.maxRotation.getOrElse(humanMaxRotation),
          followRunningProbability = soldier.followProbability.getOrElse(humanFollowProbability),
          fight = Fight(soldier.fightBackProbability, aggressive = soldier.aggressive),
          rescue = rescue,
          canLeave = false,
          function = _root_.zombies.agent.Human.Army,
          rng = random)
      }

      def generateVolunteers(redCross: Volunteer) = {
        import _root_.zombies.agent._
        val informed = random.nextDouble() < redCross.informedRatio.getOrElse(humanInformedRatio)
        val alerted = random.nextDouble() < redCross.alertedRatio.getOrElse(0.0)
        val rescue = Rescue(informProbability = redCross.informProbability.getOrElse(humanInformProbability), noFollow = true, alerted = alerted, informed = informed)
        val antidote = Antidote(activationDelay = redCross.activationDelay, immunityLossProbability = redCross.immunityLoosProbability, efficiencyProbability = redCross.efficiencyProbability, vaccinatedExhaustionProbability = redCross.vaccinatedExhaustionProbability.toOption)
        _root_.zombies.agent.Human(
          world = world,
          walkSpeedParameter = walkSpeed,
          runSpeedParameter = redCross.runSpeed.getOrElse(humanRunSpeed),
          exhaustionProbability = redCross.exhaustionProbability.getOrElse(humanExhaustionProbability),
          perceptionParameter = redCross.perception.getOrElse(humanPerception),
          maxRotation = redCross.maxRotation.getOrElse(humanMaxRotation),
          followRunningProbability = redCross.followProbability.getOrElse(humanFollowProbability),
          fight = Fight(humanFightBackProbability, aggressive = redCross.aggressive),
          rescue = rescue,
          canLeave = false,
          antidote = antidote,
          function = _root_.zombies.agent.Human.RedCross,
          rng = random)
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
        case soldier: Soldier =>
          soldier.location.toOption match {
            case Some(l) => toPosition(l).map(l => generateSoldier(soldier).copy(position = l))
            case None => Some(generateSoldier(soldier))
          }
        case volunteer: Volunteer =>
          volunteer.location.toOption match {
            case Some(l) => toPosition(l).map(l => generateVolunteers(volunteer).copy(position = l))
            case None => Some(generateVolunteers(volunteer))
          }

      }
    }

    def modifyLocation(agentGenerator: AgentGenerator, l: Optional[Location] => Optional[Location]) =
      agentGenerator match {
        case h: Human => h.copy(location = l(h.location))
        case z: Zombie => z.copy(location = l(z.location))
        case v: Volunteer => v.copy(location = l(v.location))
        case s: Soldier => s.copy(location = l(s.location))
      }


  sealed trait AgentGenerator

  case class Soldier(
    fightBackProbability: Double = 1.0,
    exhaustionProbability: AgentGenerator.Optional[Double] = None,
    perception: AgentGenerator.Optional[Double] = None,
    runSpeed: AgentGenerator.Optional[Double] = None,
    followProbability: AgentGenerator.Optional[Double] = None,
    maxRotation: AgentGenerator.Optional[Double] = None,
    informProbability: Double = 0.0,
    aggressive: Boolean = true,
    location:  AgentGenerator.Optional[Location] = None) extends AgentGenerator

  case class Volunteer(
    vaccinatedExhaustionProbability: AgentGenerator.Optional[Double] = None,
    followProbability: AgentGenerator.Optional[Double] = None,
    informProbability: AgentGenerator.Optional[Double] = None,
    aggressive: Boolean = false,
    activationDelay: Int,
    efficiencyProbability: Double = 1.0,
    immunityLoosProbability: Double = 0.0,
    fightBackProbability: AgentGenerator.Optional[Double] = None,
    exhaustionProbability:  AgentGenerator.Optional[Double] = None,
    perception: AgentGenerator.Optional[Double] = None,
    runSpeed: AgentGenerator.Optional[Double] = None,
    informedRatio: AgentGenerator.Optional[Double] = None,
    maxRotation: AgentGenerator.Optional[Double] = None,
    alertedRatio: AgentGenerator.Optional[Double] = None,
    location:  AgentGenerator.Optional[Location] = None) extends AgentGenerator


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

  type EntranceLaw = agent.Agent.EntranceLaw

  implicit class EntranceLawParameterDecorator(e: EntranceLaw.Parameter) {
    def around(range: Double) = {
      val cellSide = space.cellSide(e.world.side)
      Agent.around(_root_.zombies.world.World.cellCenter(e.world, e.entranceLocation), range * cellSide, e.index, e.neighborhoodCache).toVector
    }

    def visible =
      Agent.around(_root_.zombies.world.World.cellCenter(e.world, e.entranceLocation), e.simulation.relativeHumanPerception, e.index, e.neighborhoodCache).toVector

    def enter(generator: Iterable[AgentGenerator]) = generator.flatMap { generator =>
      AgentGenerator.generateAgent(
        generator = AgentGenerator.modifyLocation(generator, l => Some(l.getOrElse(e.entranceLocation))),
        walkSpeed = e.simulation.walkSpeedParameter,
        humanRunSpeed = e.simulation.humanRunSpeed,
        humanPerception = e.simulation.humanPerception,
        humanMaxRotation = e.simulation.humanMaxRotation,
        humanExhaustionProbability = e.simulation.humanExhaustionProbability,
        humanFollowProbability = e.simulation.humanFollowProbability,
        humanInformedRatio = e.simulation.humanInformedRatio,
        humanInformProbability = e.simulation.humanInformProbability,
        humanFightBackProbability = e.simulation.humanFightBackProbability,
        zombieRunSpeed = e.simulation.zombieRunSpeed,
        zombiePerception = e.simulation.zombiePerception,
        zombieMaxRotation = e.simulation.zombieMaxRotation,
        zombieCanLeave = e.simulation.zombieCanLeave,
        world = e.world,
        random = e.random
      )
    }.toVector
  }

  def poison(lambda: Double, random: Random) = EntranceLaw.poison(lambda, random)

  val CaptureTrap = world.CaptureTrap
  val DeathTrap = world.DeathTrap

  implicit class TrapDecorator(w: World) {
    def withTrap(t: (Location, world.Trap)*) = world.World.setTraps(w, t)
  }




object api extends DSL