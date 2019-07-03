package zombies.ode

import better.files._

object ODEModule extends App {

    val simures = ModelModule.run(
      // ODE parameters
      infection = 1,
      information = 10,
      rescue = 0.5,
      alpha = 5,
      death = 0.25,
      // Initial conditions
      statesInit = Vector(250.0, 0.0, 4.0, 0.0),
      // Time steps
      //t0 = 1,
      //dt = 0.01,
      tMax = 500,
      //tWarp = 500
    )//.mkString("\n")

  println(simures)
  //println(simures._1.size)

}

object ModelModule {

  def interpolate(x: Vector[Double], doubleind: Double) = {
    if (doubleind.toInt==doubleind) x(doubleind.toInt)
    else {
      assert(doubleind > 0 && doubleind < x.size, s"Double index out of bounds : ${doubleind} for size ${x.size}")
      val ileft = math.floor(doubleind).toInt
      if (ileft == x.size-1) x(ileft)
      else {
        val (left, right) = (x(ileft), x(ileft + 1))
        val w = doubleind - math.floor(doubleind)
        left + w * (right - left)
      }
    }
  }

  def run(infection: Double, information: Double, rescue: Double, alpha: Double, death: Double,
          statesInit: Vector[Double],
          t0: Int = 1, dt: Double = 0.01, tMax: Int = 500, tWarp: Int = 484,
          ABMTimeSerieSteps: Int = 500
         ) = {
    val nbIntervals = ((tMax - t0) / dt).toInt

    // Simulation data
    val simul = integrate(dynamic(infection, information, rescue, alpha, death))(t0, dt, nbIntervals, List(statesInit))
    val Vector(humansUninformed, humansInformed, zombified, rescued) = simul.toVector.transpose

    // Sampling over simulation data
    val maxIndSampling = (tWarp - t0) / dt
    val samplingStep = maxIndSampling / ABMTimeSerieSteps
    val samplingSteps = (0.0 to maxIndSampling by samplingStep)

    val humansUninformedSampled = samplingSteps.map(interpolate(humansUninformed,_))
    val humansInformedSampled = samplingSteps.map(interpolate(humansInformed,_))
    val zombifiedSampled = samplingSteps.map(interpolate(zombified,_))
    val rescuedSampled = samplingSteps.map(interpolate(rescued,_))

    (humansUninformedSampled, humansInformedSampled, zombifiedSampled, rescuedSampled)
  }

  // Description of the ODE system
  def dynamic(infection: Double, information: Double, rescue: Double, alpha: Double, death: Double)
             (t: Double, state: Vector[Double]): Vector[Double] = {
    // Param
    val N = state.sum

    // ODE system
    def dH_uninformed(state: Vector[Double]) =
      -(infection + alpha * rescue + information * state(1) / (state(0) + state(1))) * state(0)

    def dH_informed(state: Vector[Double]) =
      information * state(1) / (state(0) + state(1)) * state(0) - (infection + rescue) * state(1)

    def dZ(state: Vector[Double]) =
      infection * (state(0) + state(1)) - death * state(2)

    def dR(state: Vector[Double]) =
      rescue * (alpha * state(0) + state(1))

    // Output
    Vector(
      dH_uninformed(state),
      dH_informed(state),
      dZ(state),
      dR(state)
    )
  }


  // ODE solver
  def integrate(f: (Double, Vector[Double]) => Vector[Double])(t0: Double, dt: Double, counter: Int, ysol: List[Vector[Double]]): List[Vector[Double]] = {
    def multiply(v: Vector[Double], s: Double) = v.map(_ * s)
    def divide(v: Vector[Double], s: Double) = v.map(_ / s)
    def add(vs: Vector[Double]*) = {
      def add0(v1: Vector[Double], v2: Vector[Double]) = (v1 zip v2).map { case(a, b) => a + b }
      vs.reduceLeft(add0)
    }

    val yn = ysol.head

    if (counter > 0) {
      val dy1 = multiply(f(t0, yn), dt)
      val dy2 = multiply(f(t0 + dt / 2, add(yn, divide(dy1, 2))), dt)
      val dy3 = multiply(f(t0 + dt / 2, add(yn, divide(dy2, 2))), dt)
      val dy4 = multiply(f(t0 + dt, add(yn, dy3)), dt)
      val y = add(yn, divide(add(dy1, multiply(dy2, 2), add(multiply(dy3, 2), dy4)), 6))::ysol
      val t = t0 + dt
      integrate(f)(t, dt, counter - 1, y)
    } else ysol.reverse
  }
}
