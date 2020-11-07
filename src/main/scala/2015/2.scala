package advent2015
import common.Day
import scala.io.Source

class Day2(source: Source) extends Day {
  def input = source.getLines().map(_.split("x").map(_.toInt).sorted)

  def paperSizes = input.map{case Array(x, y, z) => 3 * x * y + 2 * x * z + 2 * y * z}

  override def answer1 = paperSizes.sum.toString

  def ribbonSizes = input.map{case Array(x, y, z) => 2 * x + 2 * y + x * y * z}

  override def answer2 = ribbonSizes.sum.toString
}
