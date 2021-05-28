package zombies.console

import zombies.simulation.{Event, Simulation, environment, physic}
import zombies.world.World
import zombies._
import scala.util.Random

object Test extends App {

  import physic._

  val humans = 250
  val zombies = 10

  val rng = new Random(42)

  val world = World.jaude
  val simulation = api.initialize(
    environment.stadium,
    humans = humans,
    zombies = zombies,
    walkSpeed = walkSpeed,
    random = rng
  )

  def display(simulation: Simulation, events: Vector[Event], allEvents: List[Event]) = {
    print(Console.display(simulation, allEvents))
    Thread.sleep(100)
    Console.clear(simulation)
    allEvents ++ events
  }

  Simulation.simulate(simulation, rng, 5000, display, List())

  //println(_root_.zombies.simulation.simulate(simulation, rng, 500).humansDynamic(1).size)
 // println(_root_.zombies.simulation.simulate(simulation, rng, 500).humansDynamic(1).size)

//  def bench(steps: Int) = simulate[Unit](simulation, rng, steps, (_, _, _) => Unit, Unit)
//
//  val begin = System.currentTimeMillis()
//  val end = bench(500)
//  println(System.currentTimeMillis() - begin)

}
