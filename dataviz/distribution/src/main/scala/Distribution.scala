
import plotly._, element._, layout._, Plotly._
import better.files._

object Distribution extends App {

  val dynamicsFile = File(args(0))

  def cumulative(f: File, column: Int) = {
    val rescued = f.lines.map(_.split(",")(column)).map(_.toDouble)
    def ceil(d: Double) = ((d / 5).ceil * 5) + 2.5
    rescued.groupBy(ceil).view.mapValues(_.size).toSeq
  }

  val c = 11
  val distribution = cumulative(dynamicsFile, c)

  Bar(distribution.map(_._1), distribution.map(_._2), width = 4.9).plot(
    title = s"Cumulative distribution, step ${c*5} - ${(c+1)*5}",
    xaxis = Axis("Rescued"),
    yaxis = Axis("Occurrences"),
    width = 800,
    height = 600
  )

}
