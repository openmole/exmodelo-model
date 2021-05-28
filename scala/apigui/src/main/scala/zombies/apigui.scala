package zombies

import zombies.agent.Agent.EntranceLaw
import zombies.api._

import scala.util.Random
import scala.scalajs.js.annotation.JSExportTopLevel

object apigui {

  @JSExportTopLevel("zombies")
  def zombies(): Unit = {

    def world =
      World {
        "++++++++++++++\n" +
        "+++++++e00000+\n" +
        "+++++++000000+\n" +
        "+R00000000000+\n" +
        "+R00000000++++\n" +
        "++++000000++++\n" +
        "++++DT0000++++\n" +
        "+000000000++++\n" +
        "+000000000++++\n" +
        "+000000000++++\n" +
        "+000000000++++\n" +
        "++++0000DD++++\n" +
        "++++0000ee++++\n" +
        "++++++++++++++"
      }

    val rng = new Random(42)
    val humanPopulation = 60

    val agents =
        (0 to 5).map(_ => Zombie(location = (1, 9), runSpeed = 0.7)) ++
        (0 to 5).map(_ => Zombie(location = (1, 9), runSpeed = 0.2))

    def entranceLaw: EntranceLaw = { context =>
      val humans = context.agents.filter(Agent.isHuman)
      val zombiesAround = context.visible.filter(Agent.isZombie)
      val enterTime = context.step % 10 == 0
      val enter = if(enterTime && zombiesAround.isEmpty) (humanPopulation - humans.size) / 3 else 0
      val agents =  (0 until enter).map { i => Human() }
      context.enter(agents)
    }

    def init(random: Random) =
      initialize(
        world = world,
        zombies = 0,
        humans = 0,
        agents = agents,
        entrance = entranceLaw,
        random = random
      )

    display.init(
      () => init(rng),
      List.empty)
  }

}
