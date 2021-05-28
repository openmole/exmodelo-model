package zombies.ode

import better.files._

object ODE extends App {

    val simures = Model.run(
      // Real data
      //file = File("ode/realData.csv").toJava,
      // ODE parameters
      panic0 = 1,
      staminaH = 10,
      hunt0 = 0.5,
      staminaZ = 5,
      inf0 = 0.25,
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

object Model {

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

  def run(panic0: Double, staminaH: Double, hunt0: Double, staminaZ: Double, inf0: Double,
          out0: Double = 0.0, fightback: Double = 0.0, die0: Double = 0.0,
          statesInit: Vector[Double],
          t0: Int = 1, dt: Double = 0.01, tMax: Int = 500, tWarp: Int = 484,
          ABMTimeSerieSteps: Int = 500
         ) = {
    val exhaustH = 1.0 / staminaH
    val exhaustZ = 1.0 / staminaZ
    val nbIntervals = ((tMax - t0) / dt).toInt

    // Simulation data
    val simul = integrate(dynamic(panic0, exhaustH, hunt0, exhaustZ, inf0, out0, fightback, die0))(t0, dt, nbIntervals, List(statesInit))
    val Vector(humansWalking, humansRunning, zombifiedWalking, zombifiedRunning) = simul.toVector.transpose

    // Sampling over simulation data
    val maxIndSampling = (tWarp - t0) / dt
    val samplingStep = maxIndSampling / ABMTimeSerieSteps
    val samplingSteps = (BigDecimal(0.0) to maxIndSampling by samplingStep) map {_.toDouble}

    val humansWalkingSampled = samplingSteps.map(interpolate(humansWalking,_))
    val humansRunningSampled = samplingSteps.map(interpolate(humansRunning,_))
    val zombifiedWalkingSampled = samplingSteps.map(interpolate(zombifiedWalking,_))
    val zombifiedRunningSampled = samplingSteps.map(interpolate(zombifiedRunning,_))

    (humansWalkingSampled, humansRunningSampled, zombifiedWalkingSampled, zombifiedRunningSampled)
  }

  // Description of the ODE system
  def dynamic(panic0: Double, exhaustH: Double, hunt0: Double, exhaustZ: Double, inf0: Double,
              out0: Double = 0.0, fightback: Double = 0.0, die0: Double = 0.0)
             (t: Double, state: Vector[Double]): Vector[Double] = {
    // Param
    val N = state.sum
    val panic = panic0 * (state(2) + state(3)) / N
    val hunt = hunt0 * (state(0) + state(1)) / N
    val inf = inf0 * (state(2) + state(3)) / N * (1 - fightback)
    val out = out0 * (state(0) + state(1)) / N
    val die = die0 * (state(0) + state(1)) / N

    // ODE system
    def dH_walk(state: Vector[Double]) =
      -(panic + inf + out) * state(0) + exhaustH * state(1)

    def dH_run(state: Vector[Double]) =
      panic * state(0) - (exhaustH + inf + out) * state(1)

    def dZ_walk(state: Vector[Double]) =
      inf * (state(0) + state(1)) - (hunt + die) * state(2) + exhaustZ * state(3)

    def dZ_run(state: Vector[Double]) =
      hunt * state(2) - (exhaustZ + die) * state(3)

    // Output
    Vector(
      dH_walk(state),
      dH_run(state),
      dZ_walk(state),
      dZ_run(state)
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
