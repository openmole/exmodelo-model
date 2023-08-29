package zombies.console

import zombies.simulation.{Event, Simulation, environment, physic}
import zombies.world.{Neighborhood, World}
import zombies.*
import zombies.Console.EventCount

import scala.util.Random

object Test extends App {

  import physic._

  val humans = 250
  val zombies = 10

  val rng = new Random(42)

  val simulation = api.initialize(
    environment.stadium,
    humans = humans,
    zombies = zombies,
    walkSpeed = walkSpeed,
    random = rng,
    neighborhood = Neighborhood.All
  )

  var count = EventCount(0, 0, 0)

  def display(step: Int, simulation: Simulation, events: Vector[Event]) = 
    print(Console.display(step, simulation, count))
    Thread.sleep(100)
    Console.clear(simulation)
    count =
      EventCount(
        rescued = count.rescued + events.collect(Event.rescued).size,
        killed = count.rescued + events.collect(Event.killed).size,
        zombified = count.rescued + events.collect(Event.zombified).size,
      )

  Simulation.listenSimulation(simulation, rng, 5000, display)

  //Simulation.simulateBlind(simulation, rng, 5000)


  //println(_root_.zombies.simulation.simulate(simulation, rng, 500).humansDynamic(1).size)
 // println(_root_.zombies.simulation.simulate(simulation, rng, 500).humansDynamic(1).size)

//  def bench(steps: Int) = simulate[Unit](simulation, rng, steps, (_, _, _) => Unit, Unit)
//
//  val begin = System.currentTimeMillis()
//  val end = bench(500)
//  println(System.currentTimeMillis() - begin)

}
