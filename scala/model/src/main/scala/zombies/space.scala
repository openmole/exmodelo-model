package zombies

import zombies.world.World

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.scalajs.js.annotation._
import scala.util.Random

@JSExportTopLevel("space")
object space {

  def neighbors[T](get: (Int, Int) => Iterable[T], x: Int, y: Int, neighborhoodSize: Int, center: Boolean = true) = {
    val res = ListBuffer[T]()

    for {
      ox <- -neighborhoodSize to neighborhoodSize
      oy <- -neighborhoodSize to neighborhoodSize
      if center || ox != oy
    } res ++= get(x + ox, y + oy)

    res.toList
  }


  def cellSide(n: Int) = (1.0 / n)
  def cellDiagonal(n: Int) = math.sqrt(2.0) * cellSide(n)

  def distance(x1: (Double, Double), x2: (Double, Double)) =
    math.sqrt(math.pow(x1._1 - x2._1, 2) + math.pow(x1._2 - x2._2, 2))

  def closest[T](t: T, v: Iterable[T], position: T => Position) =
    v.minBy(v => distance(position(t), position(v)))

  def direction(x1: (Double, Double), x2: (Double, Double)) =
    (x2._1 - x1._1, x2._2 - x1._2)

  def opposite(v: (Double, Double)) = (-v._1, -v._2)

  def rotate(v: (Double, Double), theta: Double) = {
    import math._
    val (x, y) = v
    (x * cos(theta) - y * sin(theta), x * sin(theta) + y * cos(theta))
  }

  def get[T](a: Array[Array[T]], x: Int, y: Int) =
    if(x < 0 || x >= a.size || y < 0 || y >= a(x).size) None
    else Some(a(x)(y))

  type Location = (Int, Int)
  type Position = (Double, Double)
  type Velocity = (Double, Double)

  def length(v: (Double, Double)) = {
    val (x, y) = v
    math.sqrt(x * x + y * y)
  }

  def normalize(v: (Double, Double), norm: Double = 1.0) = {
    val (x, y) = v
    val l =  length(v)
    if(l == 0) v else (x * norm / l, y * norm / l)
  }

  def bound(v: (Double, Double), min: Double, max: Double): (Double, Double) = {
    val l = length(v)
    if (l < min) normalize(v, min)
    else if (l > max) normalize(v, max)
    else v
  }

  def sum(v1: (Double, Double), v2: (Double, Double)): (Double, Double) = {
    val (x, y) = v1
    val (dx, dy) = v2
    (x + dx, y + dy)
  }

  def sum(v: Seq[(Double, Double)]): (Double, Double) = v.foldLeft((0.0, 0.0))(sum(_, _))
  def average(v: Seq[(Double, Double)]): (Double, Double) = {
    val (x, y) = sum(v)
    val s = v.size
    (x / s, y / s)
  }

  def diff(v1: (Double, Double), v2: (Double, Double)) = (v2._1 - v1._1, v2._2 - v1._2)

  def randomUnitVector(rng: Random) = (rng.nextDouble(), rng.nextDouble())



  def positionToLocation(v: Position, side: Int): Location = {
    val (x, y) = v
    ((side * x).toInt, (side * y).toInt)
  }

  object Index {

    def apply[T: ClassTag](content: Iterable[T], location: T => Location, side: Int): Index[T] = {
      val cellBuffer: Array[Array[ArrayBuffer[T]]] = Array.fill(side, side) {
        ArrayBuffer[T]()
      }

      for {
        s <- content
        (i, j) = location(s)
      } cellBuffer(i)(j) += s

      Index[T](cellBuffer.map(_.map(_.toArray)), side)
    }


    def get[T](index: Index[T], x: Int, y: Int) =
      if(x > 0 && x < index.side && y > 0 && y < index.side) index.cells(x)(y).toTraversable
      else Iterable.empty


  }

  case class Index[T](cells: Array[Array[Array[T]]], side: Int)

}
