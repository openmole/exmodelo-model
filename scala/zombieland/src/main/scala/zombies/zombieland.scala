package zombies

import zombies.guitutils.parameters._
import scala.scalajs.js.annotation.JSExportTopLevel

object zombieland {
  @JSExportTopLevel("zombies")
  def zombies(): Unit = {
    simulate.buildGUI(
      ()=> world.World.jaude,
      infectionRange,
      walkSpeed,
      humanMaxRotation,
      humanPerception,
      humanStamina,
      humanRunSpeed,
      humanFollowModeProbability,
      humanAwarenessProbability,
      humanInformedRatio,
      numberHumans,
      zombieMaxRotation,
      zombiePerception,
      zombieStamina,
      zombieRunSpeed,
      numberZombies
    )
  }
}