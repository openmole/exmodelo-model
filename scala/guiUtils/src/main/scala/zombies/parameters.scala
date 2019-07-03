package zombies.guitutils

import zombies.simulation._
import zombies.guitutils.controls.Mecanism
import shapeless._
import rx._

object parameters {

  type ParameterName = String

  object Parameter {
    def range: PartialFunction[Parameter, Range[_]] = {
      case r: Range[_] => r
    }

    //
    //
    //        def activation(p: Parameter) = p match {
    //          case Range(_,_, activation, onoff)=>
    //            if (activation != Off)
    //              onoff.map{_.activeInSimulation}
    //          case OnOff(_, _, activation)=> activation.now
    //          case o: Options=> o.activation
    //        }
    //      }
  }

  sealed trait Parameter {
    def activation: Activation
  }

  case class Options(name: ParameterName, mecanisms: Seq[controls.Mecanism], default: Mecanism, off: Mecanism, activation: Activation) extends Parameter

  case class OnOff[T](name: ParameterName, activeInSimulation: Boolean, activation: Activation, childs: Seq[ParameterName]) extends Parameter {
    def isOn = copy(activation = Variable)
    def isOff = copy(activation = Off)
  }

  case class Range[T](name: ParameterName, value: RangeValue[T], activation: Activation) extends Parameter {
    def isDefault = copy(activation = Default)
    def asDefaultFrom(parameter: Range[T]) = parameter.copy(value = from(parameter), activation = Default)
    def isOff = copy(activation = Off)
    def from(aParameter: Range[T]) = aParameter.value
    def withDefault(v: T) = copy(value = value.copy(default = v))
  }


  def value(p: Parameter) = {

  }


  def isVariable(p: Parameter) = p match {
    case o: Options => o.activation == Variable
    case oo: OnOff[_] => oo.activation == Variable
    case r: Range[_] => r.activation == Variable
  }

  def defaultOrOff[T](parameter: Range[T]) = {
    if (parameter.activation == Off) parameter.value.off
    else parameter.value.default
  }

  def defaultOrOff(parameter: Options): Mecanism = {
    if (parameter.activation == Off) parameter.off
    else parameter.default
  }

  trait Activation

  object Variable extends Activation

  object Off extends Activation

  object Default extends Activation

  object Range {
    val caseInt = TypeCase[Range[Int]]
    val caseDouble = TypeCase[Range[Double]]
  }

  case class RangeValue[T](min: T, max: T, step: T, default: T, off: T)


  val numberZombies = Range("numberZombies", RangeValue(0, 100, 1, 4, 0), Variable)
  val numberHumans = Range("numberHumans", RangeValue(0, 1500, 1, 250, 0), Variable)

  val infectionRange = Range("infectionRange", RangeValue(0.0, 1.0, 0.01, physic.infectionRange, 0.0), Variable)
  val walkSpeed = Range("walkSpeed", RangeValue(0.0, 1.0, 0.1, physic.walkSpeed, 0.0), Variable)

  val zombieRunSpeed = Range("zombieRunSpeed", RangeValue(0.0, 1.0, 0.01, physic.zombieRunSpeed, 0.0), Variable)
  val zombiePerception = Range("zombiePerception", RangeValue(0.0, 5.0, 0.01, physic.zombiePerception, 0.0), Variable)
  val zombieMaxRotation = Range("zombieMaxRotation", RangeValue(0.0, 180.0, 1.0, physic.zombieMaxRotation, 0.0), Variable)
  val zombiePheromoneEvaporation = Range("zombiePheromoneEvaporation", RangeValue(0.0, 5.0, 0.01, physic.zombiePheromoneEvaporation, 0.0), Variable)

  val humanRunSpeed = Range("humanRunSpeed", RangeValue(0.0, 1.0, 0.01, physic.humanRunSpeed, 0.0), Variable)
  val humanPerception = Range("humanPerception", RangeValue(0.0, 5.0, 0.01, physic.humanPerception, 0.0), Variable)
  val humanMaxRotation = Range("humanMaxRotation", RangeValue(0.0, 180.0, 1.0, physic.humanMaxRotation, 0.0), Variable)
  val humanExhaustionProbability = Range("humanExhaustionProbability", RangeValue(0.0, 1.0, 0.01, physic.humanExhaustionProbability, 0.0), Variable)
  val humanFightBackProbability = Range("humanFightBackProbability", RangeValue(0.0, 1.0, 0.01, physic.humanFightBackProbability, 0.0), Variable)
  val humanFollowProbability = Range("humanFollowProbability", RangeValue(0.0, 1.0, 0.01, physic.humanFollowProbability, 0.0), Variable)
  val humanInformProbability = Range("humanInformProbability", RangeValue(0.0, 1.0, 0.01, physic.humanInformProbability, 0.05), Variable)
  val humanInformedRatio = Range("humanInformedRatio", RangeValue(0.0, 1.0, 0.01, physic.humanInformedRatio, 0.0), Variable)

  val armySize = Range("armySize", RangeValue(0, 50, 1, 10, 0), Variable)
  val armyFightBackProbability = Range("armyFightBackProbability", RangeValue(0.0, 1.0, 0.01, 1.0, 0.0), Variable)
  val armyExhaustionProbability = Range("armyExhaustionProbability", RangeValue(0.0, 1.0, 0.01, physic.humanExhaustionProbability, 0.0), Variable)
  val armyPerception = Range("armyPerception", RangeValue(0.0, 5.0, 0.01, physic.humanPerception, 0.0), Variable)
  val armyRunSpeed = Range("armyRunSpeed", RangeValue(0.0, 1.0, 0.01, physic.humanRunSpeed, 0.0), Variable)
  val armyFollowProbability = Range("armyFollowProbability", RangeValue(0.0, 1.0, 0.01, physic.humanFollowProbability, 0.0), Variable)
  val armyMaxRotation = Range("armyMaxRotation", RangeValue(0.0, 180.0, 1.0, physic.humanMaxRotation, 0.0), Variable)
  val armyInformProbability = Range("armyInformProbability", RangeValue(0.0, 1.0, 0.01, 0.0, 0.05), Variable)
  val armyAggressive = OnOff("armyAggressive", true, Variable, Seq())


  val redCrossSize = Range("redCrossSize", RangeValue(0, 50, 1, 10, 0), Variable)
  val redCrossExhaustionProbability = Range("redCrossExhaustionProbability", RangeValue(0.0, 1.0, 0.01, physic.humanExhaustionProbability, 0.0), Variable)
  val redCrossExhaustionMechanism = OnOff("redCrossExhaustion", false, Variable, Seq(redCrossExhaustionProbability.name))
  val redCrossFollowProbability = Range("redCrossFollowProbability", RangeValue(0.0, 1.0, 0.01, physic.humanFollowProbability, 0.0), Variable)
  val redCrossInformProbability = Range("redCrossInformProbability", RangeValue(0.0, 1.0, 0.01, 0.0, 0.05), Variable)
  val redCrossAggressive = OnOff("redCrossAggressive", false, Variable, Seq())
  val activationDelay = Range("redCrossInformProbability", RangeValue(0, 250, 1, 50, 0), Variable)
  val efficiencyProbability = Range("redCrossEfficiencyProbability", RangeValue(0.0, 1.0, 0.01, 0.1, 0.0), Variable)

  val armyOnOff = OnOff("army", false, Variable,
    Seq(armySize, armyFightBackProbability, armyExhaustionProbability, armyPerception, armyRunSpeed, armyFollowProbability, armyMaxRotation, armyInformProbability).map {
      _.name
    } :+ armyAggressive.name)

  val redCrossOnOff = OnOff("red cross", false, Variable,
    Seq(redCrossSize, redCrossFollowProbability, redCrossInformProbability, activationDelay, efficiencyProbability).map {
      _.name
    } ++ Seq(redCrossAggressive.name, redCrossExhaustionMechanism.name))

  //val army = Army(4, fightBackProbability = 0.99, exhaustionProbability = 0.1, perception = 4.0, runSpeed = 0.9, followRunning = 0.05, maxRotation = 180)

  /**
    * Spatial generator parameters
    */

  val generationMethod = Options("generationMethod", Seq("random", "expMixture", "blocks", "percolation"), "blocks", "Jaude", Variable)
  //    val gridSize = Range("gridSize", RangeValue(10, 100, 1, 40, 40), Variable)
  /**
    * Random
    */
  val randomDensity = Range("randomDensity", RangeValue(0.0, 1.0, 0.01, 0.5, 0.5), Variable)

  /**
    * ExpMixture
    */
  val expMixtureCenters = Range("expMixtureCenters", RangeValue(1, 100, 1, 5, 5), Variable)
  val expMixtureRadius = Range("expMixtureRadius", RangeValue(0.0, 10.0, 0.1, 5.0, 5.0), Variable)
  val expMixtureThreshold = Range("expMixtureThreshold", RangeValue(0.0, 1.0, 0.01, 0.5, 0.5), Variable)
  /**
    * blocks
    */
  val blocksNumber = Range("blocksNumber", RangeValue(1, 10, 1, 5, 5), Variable)
  val blocksMinSize = Range("blocksMinSize", RangeValue(1, 10, 1, 5, 5), Variable)
  val blocksMaxSize = Range("blocksMaxSize", RangeValue(1, 20, 1, 15, 15), Variable)
  /**
    * percolation
    */
  val percolationProba = Range("percolationProba", RangeValue(0.0, 1.0, 0.01, 0.2, 0.2), Variable)
  val percolationBordPoints = Range("percolationBordPoints", RangeValue(1, 100, 1, 20, 20), Variable)
  val percolationLinkWidth = Range("percolationLinkWidth", RangeValue(1.0, 5.0, 0.1, 3.0, 3.0), Variable)


}