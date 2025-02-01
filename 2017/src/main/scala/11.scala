package advent2017
import common.Day
import scala.io.Source
import Math.abs

class Day11(source: Source) extends Day {
  val input = source.mkString.split(",").map(_.trim)

  val offsets = Map(
    "n"  -> ( 0,  1, -1),
    "s"  -> ( 0, -1,  1),
    "ne" -> ( 1,  0, -1),
    "sw" -> (-1,  0,  1),
    "nw" -> (-1,  1,  0),
    "se" -> ( 1, -1,  0)
  )

  val coords = input.scanLeft((0, 0, 0)){ case ((x, y, z), dir) =>
    val (xOff, yOff, zOff) = offsets(dir)
    (x + xOff, y + yOff, z + zOff)
  }

  def distance(coords: (Int, Int, Int)): Int = List(abs(coords._1), abs(coords._2), abs(coords._3)).max

  override def answer1 = distance(coords.last).toString
  override def answer2 = coords.map(distance).max.toString
}
