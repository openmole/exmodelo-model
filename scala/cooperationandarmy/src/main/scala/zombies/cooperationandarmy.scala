package zombies

import zombies.guitutils.parameters._
import scala.scalajs.js.annotation.JSExportTopLevel

object cooperation {
  @JSExportTopLevel("zombies")
  def zombies(): Unit = {
    simulate.buildGUI(
      (_, _)=> simulation.environment.quarantine,
      humanPerception isDefault,
      zombiePerception isDefault,
      humanRunSpeed asDefaultFrom(walkSpeed),
      zombieRunSpeed asDefaultFrom(walkSpeed),
      infectionRange.isDefault,
      humanExhaustionProbability isDefault,
      humanFightBackProbability isDefault,
      humanFollowProbability withDefault(0.5),
      humanInformProbability isDefault,
      numberHumans isDefault,
      numberZombies isDefault,
      humanMaxRotation isOff,
      humanInformProbability withDefault(0.5),
      humanInformedRatio withDefault(0.5),
      zombieMaxRotation isOff,

      armyOnOff,
      armySize,
      armyFightBackProbability,
      armyExhaustionProbability,
      armyPerception,
      armyRunSpeed,
      armyFollowProbability,
      armyMaxRotation,
      armyInformProbability,
      armyAggressive
    )
  }
}
