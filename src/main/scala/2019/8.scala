package advent2019
import common.Day
import scala.io.Source

class Day8(source: Source) extends Day {
  val width = 25
  val height = 6

  val layers = source.getLines().next().grouped(width * height).toList

  def mergeLayers(top: String, bottom: String): String =
    top.zip(bottom).map{
      case ('2', b) => b
      case (t,   _) => t
    }.mkString

  val mergedLayers: String = layers.reduceLeft(mergeLayers)

  override def answer1 = {
    val layer = layers.minBy(_.count(_ == '0'))
    val ones = layer.count(_ == '1')
    val twos = layer.count(_ == '2')
    (ones * twos).toString
  }

  override def answer2 = mergedLayers.map{x => if (x == '0') ' ' else '█'}.grouped(width).mkString("\n")
}
