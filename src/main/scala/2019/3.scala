package advent2019
import common.Day
import scala.io.Source

class Day3(source: Source) extends Day {
  val input = source.getLines.map(_.split(",").map(x => (x.head, x.tail.toInt)).toList).toVector

  @scala.annotation.tailrec
  private def path(directions: List[(Char, Int)], x: Int = 0, y: Int = 0, acc: List[(Int, Int)] = List.empty): List[(Int, Int)] = {
    if (directions.isEmpty) {
      acc
    } else {
      val ((direction, count) :: tail) = directions
      if (count == 0) {
        path(tail, x, y, acc)
      } else {
        direction match {
          case 'U' => path((direction, count - 1) :: tail, x, y - 1, (x, y) :: acc)
          case 'D' => path((direction, count - 1) :: tail, x, y + 1, (x, y) :: acc)
          case 'L' => path((direction, count - 1) :: tail, x - 1, y, (x, y) :: acc)
          case 'R' => path((direction, count - 1) :: tail, x + 1, y, (x, y) :: acc)
        }
      }
    }
  }

  val closestIntersection = (path(input(0)).toSet.intersect(path(input(1)).toSet) - ((0, 0))).map{case (x, y) => math.abs(x) + math.abs(y)}.min

  override def answer1 = closestIntersection.toString

  override def answer2 = (path(input(0)).zipWithIndex.drop(1) ++ path(input(1)).zipWithIndex.drop(1)).groupBy{case ((x, y), index) => (x, y)}.filter(_._2.size > 1).map{_._2.map{_._2}.sum}.min.toString
  // 4142 is too low
}
