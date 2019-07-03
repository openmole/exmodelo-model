package zombies

import zombies.guitutils.parameters._
import scala.scalajs.js.annotation.JSExportTopLevel

object zombieland {
  @JSExportTopLevel("zombies")
  def zombies(): Unit = {
    simulate.buildGUI(
      (_, _) => simulation.environment.quarantine,
      infectionRange,
      walkSpeed,

      humanMaxRotation,
      humanPerception,
      humanExhaustionProbability,
      humanFightBackProbability,
      humanRunSpeed,
      humanFollowProbability,
      humanInformProbability,
      humanInformedRatio,
      numberHumans,

      zombieMaxRotation,
      zombiePerception,
      zombieRunSpeed,
      zombiePheromoneEvaporation,
      numberZombies,

      armyOnOff,
      armySize,
      armyFightBackProbability,
      armyExhaustionProbability,
      armyPerception,
      armyRunSpeed,
      armyFollowProbability,
      armyMaxRotation,
      armyInformProbability,
      armyAggressive,

      redCrossOnOff,
      redCrossSize,
      redCrossExhaustionMechanism,
      redCrossExhaustionProbability,
      redCrossFollowProbability,
      redCrossInformProbability,
      redCrossAggressive,
      activationDelay,
      efficiencyProbability
    )
  }

}