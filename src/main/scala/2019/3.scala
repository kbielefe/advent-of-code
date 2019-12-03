package advent2019
import common.Day
import scala.io.Source

class Day3(source: Source) extends Day {
  val input = source.getLines.map(_.split(",").map(x => (x.head, x.tail.toInt))).toVector

  def segmentSquares(x: Int, y: Int, direction: Char, distance: Int): Set[(Int, Int)] = direction match {
    case 'U' => ((y - distance) to y).map(y => ((x, y))).toSet
    case 'D' => (y to (y + distance)).map(y => ((x, y))).toSet
    case 'L' => ((x - distance) to x).map(x => ((x, y))).toSet
    case 'R' => (x to (x + distance)).map(x => ((x, y))).toSet
  }

  @scala.annotation.tailrec
  private def wireSquares(directions: Array[(Char, Int)], acc: Set[(Int, Int)] = Set.empty, x: Int = 0, y: Int = 0): Set[(Int, Int)] = {
    if (directions.isEmpty) {
      acc
    } else {
      val (direction, distance) = directions.head
      val (newX, newY) = direction match {
        case 'U' => (x, y - distance)
        case 'D' => (x, y + distance)
        case 'L' => (x - distance, y)
        case 'R' => (x + distance, y)
      }
      wireSquares(directions.tail, acc ++ segmentSquares(x, y, direction, distance), newX, newY)
    }
  }


  val closestIntersection = (wireSquares(input(0)).intersect(wireSquares(input(1))) - ((0, 0))).map{case (x, y) => math.abs(x) + math.abs(y)}.min

  override def answer1 = closestIntersection.toString

  override def answer2 = "unimplemented"
}
