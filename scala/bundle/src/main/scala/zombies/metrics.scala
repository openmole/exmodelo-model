package zombies

import org.apache.commons.math3.linear.MatrixUtils
import zombies.agent.{Agent, Metabolism}
import zombies.simulation.{Event, SimulationResult}
import zombies.space.Position

import scala.math._
import org.apache.commons.math3.stat.regression.SimpleRegression
import org.locationtech.jts.geom._

object metrics {

  def cumSum(v: Seq[Int]) =
    v.foldLeft(List(0)) { (acc, el)=> (el + acc.head) :: acc  }.reverse.toVector


  def zombified (results : SimulationResult): Vector[Position] = {
    results._2.flatten.collect(Event.zombified).map{ case s => s.human.position }.toVector
  }

  def hitmap(results : SimulationResult, eventLocation: SimulationResult => Vector[Position]) = {
    val world = results._1.head.world
    val worldIntPos = eventLocation(results).map{ p => space.positionToLocation(p, world.side) }.groupBy(identity).mapValues(_.size)

    Array.tabulate[Int](world.side, world.side) { (x, y) => worldIntPos.getOrElse((x, y), 0) }
  }

  def spatialStructureObservable(eventLocation:  SimulationResult => Vector[Position])(spatialIndicator: Array[Array[Double]]=>Double)(results: SimulationResult): Double =
    spatialIndicator(hitmap(results,eventLocation).map{_.map{_.toDouble}})

  def spatialMoran(eventLocation:  SimulationResult => Vector[Position])(results: SimulationResult): Double =
    spatialStructureObservable(eventLocation)(worldgen.Morphology.moranDirect)(results)

  def spatialDistanceMean(eventLocation: SimulationResult => Vector[Position])(results: SimulationResult): Double =
    spatialStructureObservable(eventLocation)(worldgen.Morphology.distanceMeanDirect)(results)

  def spatialEntropy(eventLocation: SimulationResult => Vector[Position])(results: SimulationResult): Double =
    spatialStructureObservable(eventLocation)(worldgen.Morphology.entropy)(results)

  def spatialSlope(eventLocation: SimulationResult => Vector[Position])(results: SimulationResult): Double =
    spatialStructureObservable(eventLocation)(SpatStat.slope)(results)

  def spatialRipley(eventLocation: SimulationResult => Vector[Position])(results: SimulationResult): Double =
    spatialStructureObservable(eventLocation)(SpatStat.ripleySummary)(results)

  val defaultGroupSize = 20

  def humansDynamic(results : SimulationResult, by: Int = defaultGroupSize) = agentsDynamic(results, by, Agent.human)

  def walkingHumansDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def walking = Agent.human.andThenPartial { case h if !Metabolism.isRunning(h.metabolism) => h }
    agentsDynamic(results, by, walking)
  }
  def runningHumansDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def running = Agent.human.andThenPartial { case h if Metabolism.isRunning(h.metabolism) => h }
    agentsDynamic(results, by, running)
  }

  def uninformedHumansDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def uninformed = Agent.human.andThenPartial { case h if !agent.Human.isInformed(h) => h }
    agentsDynamic(results, by, uninformed)
  }
  def informedHumansDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def informed = Agent.human.andThenPartial { case h if agent.Human.isInformed(h) => h }
    agentsDynamic(results, by, informed)
  }

  def unalertedHumansDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def unalerted = Agent.human.andThenPartial { case h if !agent.Human.isAlerted(h) => h }
    agentsDynamic(results, by, unalerted)
  }
  def alertedHumansDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def alerted = Agent.human.andThenPartial { case h if agent.Human.isAlerted(h) => h }
    agentsDynamic(results, by, alerted)
  }

  def zombiesDynamic(results: SimulationResult, by: Int = defaultGroupSize) = agentsDynamic(results, by, Agent.zombie)

  def walkingZombiesDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def walking = Agent.zombie.andThenPartial { case z if !z.pursuing => z }
    agentsDynamic(results, by, walking)
  }

  def runningZombiesDynamic(results: SimulationResult, by: Int = defaultGroupSize) = {
    def running = Agent.zombie.andThenPartial { case z if z.pursuing => z }
    agentsDynamic(results, by, running)
  }

  def rescuedDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.rescued)
  def filteredRescuedDynamic(results: SimulationResult, runSpeed: Option[Double => Boolean], informProbability: Option[Double => Boolean], by: Int = defaultGroupSize) = {
    val (simulation, events) = results
    val cellSide = space.cellSide(simulation.head.world.side)
    def filter(h: agent.Human) = {
      val r = runSpeed.map(f => f(h.metabolism.runSpeed / cellSide)).getOrElse(true)
      val i = informProbability.map(f => f(h.rescue.informProbability)).getOrElse(true)
      r && i
    }

    Array(events.head.collect(Event.rescued).filter(e => filter(e.human)).size) ++
      events.tail.map(_.collect(Event.rescued).filter(e => filter(e.human)).size).grouped(by).map(_.sum)
  }
  def killedDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.killed)
  def zombifiedDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.zombified)
  def fleeDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.flee)
  def pursueDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.pursue)
  def humansGoneDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.humanGone)
  def zombiesGoneDynamic(results: SimulationResult, by: Int = defaultGroupSize) = eventDynamic(results, by, Event.zombieGone)

  def accumulatedRescuedDynamic(results: SimulationResult, by: Int = defaultGroupSize) = accumulatedEventDynamic(results, Event.rescued)
  def totalRescued(results: SimulationResult) = totalEvents(results, Event.rescued)

//  def filteredTotalRescued(results: SimulationResult, runSpeed: Option[Double => Boolean], informProbability: Option[Double => Boolean]) = {
//    val (simulation, events) = results
//    val cellSide = space.cellSide(simulation.head.world.side)
//    def filter(h: agent.Human) =
//      for {
//        r <- runSpeed.map(f => f(h.metabolism.runSpeed / cellSide))
//        i <- informProbability.map(f => f(h))
//      } yield r && i
//
//    events.map(_.collect(Event.rescued).filter(e => filter(e.human).getOrElse(true)).size).sum
//  }

  def halfTimeRescued(results: SimulationResult) = halfTimeEvents(results, Event.rescued)
  def peakTimeRescued(results: SimulationResult, window: Int = defaultGroupSize) = peakTimeEvents(results, window, Event.rescued)
  def peakSizeRescued(results: SimulationResult, window: Int = defaultGroupSize) = peakSizeEvents(results, window, Event.rescued)

  def totalZombified(results: SimulationResult) = totalEvents(results, Event.zombified)
  def halfTimeZombified(results: SimulationResult) = halfTimeEvents(results, Event.zombified)
  def peakTimeZombified(results: SimulationResult, window: Int = defaultGroupSize) = peakTimeEvents(results, window, Event.zombified)
  def peakSizeZombified(results: SimulationResult, window: Int = defaultGroupSize) = peakSizeEvents(results, window, Event.zombified)

  private def agentsDynamic(results : SimulationResult, by: Int, e: PartialFunction[Agent, Any]) = {
    val (simulations, _) = results
    simulations.take(1).map(_.agents.collect(e).size).toArray ++ simulations.tail.map(_.agents.collect(e).size).grouped(by).map(_.last)
  }

  private def eventDynamic(results: SimulationResult, by: Int, e: PartialFunction[Event, Any]) = {
    val (_, events) = results
    Array(events.head.collect(e).size) ++ events.tail.map(_.collect(e).size).grouped(by).map(_.sum)
  }

  private def accumulatedEventDynamic(results: SimulationResult, e: PartialFunction[Event, Any]) = {
    val (_, events) = results
    cumSum(events.map(_.collect(e)).map(_.size)).toArray
  }

  private def totalEvents(results: SimulationResult, e: PartialFunction[Event, Any]) = {
    val (_, events) = results
    events.map(_.collect(e).size).sum
  }

  private def halfTimeEvents(results: SimulationResult, e: PartialFunction[Event, Any]) = {
    val (_, events) = results
    val rescuedCum = cumSum(events.map(_.collect(e)).map(_.size))
    val rescued = rescuedCum.last
    val half = rescued / 2
    rescuedCum.indexWhere(c => c >= half)
  }

  /** Return the step where number rescued is maximum over @window simulation steps */
  private def peakTimeEvents(results: SimulationResult, window: Int, e: PartialFunction[Event, Any]) = {
    val dyn = eventDynamic(results, 1, e).sliding(window).map(_.sum).toVector
    val maxRescued = dyn.max
    dyn.indexWhere(_ == maxRescued) + window / 2
  }

  /** Return the step where number rescued is maximum over @window simulation steps */
  private def peakSizeEvents(results: SimulationResult, window: Int, e: PartialFunction[Event, Any]): Int = {
    val dyn = eventDynamic(results, 1, e).sliding(window).map(_.sum).toVector
    val maxRescued = dyn.max
    val peak = math.min(math.max(0, dyn.indexWhere(_ == maxRescued) + window / 2), dyn.size - 1)
    dyn(peak)
  }


  implicit class ComposePartial[A, B](pf: PartialFunction[A, B]) {
    def andThenPartial[C](that: PartialFunction[B, C]): PartialFunction[A, C] =
      Function.unlift(pf.lift(_) flatMap that.lift)
  }


  object SpatStat {


    def slope(matrix: Array[Array[Double]]): Double = slope(matrix.flatten)

    /**
      * rank size slope
      * @param values
      * @return
      */
    def slope(values: Array[Double]): Double = {
      def distribution: Array[Double] = values.sortBy(x=> -x).filter(_ > 0)
      def distributionLog: Array[Array[Double]] = distribution.zipWithIndex.map { case (q, i) => Array(log(i + 1), log(q)) }
      val simpleRegression = new SimpleRegression(true)
      simpleRegression.addData(distributionLog)
      //(simpleRegression.getSlope(), simpleRegression.getRSquare())
      simpleRegression.getSlope()
    }

    def intermediateSlope(values: Array[Double],lowerThreshold: Double = 0.2, upperThreshold: Double = 0.8): Double = slope(values.filter{d => d > lowerThreshold & d < upperThreshold})

    /**
      * intermediate values slope for summary of the Ripley profile
      * @param raster
      * @return
      */
    def ripleySummary(raster: Array[Array[Double]]): Double = {
      val points = raster.zipWithIndex.flatMap { case (r, i) => r.zipWithIndex.flatMap { case (d, j) => Array.fill(d.toInt)((i.toDouble, j.toDouble)) } }
      intermediateSlope(ripleyKFunction(points).map {_._2})
    }

    def ripleyKFunction(pi: Array[(Double,Double)],
                        radiusSamples: Int = 50,
                        radiusValues: Int => Array[Double] = {s => Array.tabulate(s){i => (i+1)*1.0/s}}
                       ): Array[(Double,Double)] = {
      val n = pi.length
      val rvalues = radiusValues(radiusSamples)
      val distmat = euclidianDistanceMatrix(pi)
      val area = convexHullArea(pi)
      val dmax = distmat.map{_.max}.max

      rvalues.map{ r =>
        (r,area*distmat.map{_.map{d => if(d/dmax <= r) 1.0 else 0.0}.sum}.sum / (n*(n-1)))
      }
    }

    def euclidianDistanceMatrix(pi: Array[(Double,Double)]): Array[Array[Double]] = {
      val n = pi.length
      val xcoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._1)))
      val ycoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._2)))
      MatrixUtils.createRealMatrix(xcoords.subtract(xcoords.transpose()).getData.map(_.map{case x => x*x})).add(MatrixUtils.createRealMatrix(xcoords.subtract(ycoords.transpose()).getData.map(_.map{case x => x*x}))).getData.map{_.map{math.sqrt(_)}}
    }

    def convexHullPoints(pi: Array[(Double,Double)]): Geometry = {
      val geomFactory = new GeometryFactory
      geomFactory.createMultiPoint(pi.map{case (x,y)=>geomFactory.createPoint(new Coordinate(x,y))}).convexHull
    }

    def convexHullArea(pi: Array[(Double,Double)]): Double = convexHullPoints(pi).getArea


  }



}
