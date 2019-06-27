package zombies

import zombies.api._

import scala.util.Random
import scala.scalajs.js.annotation.JSExportTopLevel

object apigui {

  @JSExportTopLevel("zombies")
  def zombies(): Unit = {

    def world =
      World {
        "++++++++++++++\n" +
        "+++++++e000000\n" +
        "+++++++0000000\n" +
        "+0000000000000\n" +
        "+000000000++++\n" +
        "++++000000++++\n" +
        "++++DTRR00++++\n" +
        "0000000000++++\n" +
        "0000000000++++\n" +
        "0000000000++++\n" +
        "0000000000++++\n" +
        "++++0000TD++++\n" +
        "++++0000ee++++\n" +
        "++++++++++++++"
      }

    val rng = new Random(42)

    val agents =
      (0 until 50).map(_ => Human(location = (7, 7))) ++
        (0 until 50).map(_ => Human(location = (7, 7))) ++
        (0 to 5).map(_ => Zombie(location = (1, 9), runSpeed = 0.7)) ++
        (0 to 5).map(_ => Zombie(location = (1, 9), runSpeed = 0.2))

    def init(random: Random) =
      initialize(
        world = world,
        zombies = 0,
        humans = 0,
        agents = agents,
        random = rng
      )

    display.init(() => init(rng), List.empty)
  }

}
