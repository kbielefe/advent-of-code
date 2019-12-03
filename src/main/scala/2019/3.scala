package advent2019
import common.Day
import scala.io.Source

class Day3(source: Source) extends Day {
  val input = source.getLines.map(_.split(",").map(x => (x.head, x.tail.toInt))).toVector

  def segmentSquares(x: Int, y: Int, direction: Char, distance: Int): Set[(Int, Int)] = direction match {
    case 'U' => (y to (y - distance)).map(y => ((x, y))).toSet
    case 'D' => (y to (y + distance)).map(y => ((x, y))).toSet
    case 'L' => (x to (x - distance)).map(x => ((x, y))).toSet
    case 'R' => (x to (x + distance)).map(x => ((x, y))).toSet
  }

  def wireSquares(directions: Array[(Char, Int)]): Set[(Int, Int)] = ???
    //directions.scanLeft((0, 0)){case ((x, y), (dir, distance)) =>

  val closestIntersection = (wireSquares(input(0)).intersect(wireSquares(input(1))) - ((0, 0))).map{case (x, y) => x + y}.min

  override def answer1 = closestIntersection.toString

  override def answer2 = ???
}
