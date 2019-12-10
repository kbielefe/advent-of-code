package advent2019
import common.Day
import scala.io.Source

class Day10(source: Source) extends Day {
  val grid = source.getLines.zipWithIndex.flatMap{case (line, y) => line.zipWithIndex.filter(_._1 == '#').map{case (_, x) => (x, y)}}.toSet
  val maxX = grid.map(_._1).max
  val maxY = grid.map(_._2).max

  @scala.annotation.tailrec
  final def visibility(from: (Int, Int), grid: Set[(Int, Int)], distance: Int = 1, count: Int = 0): Int = {
    val (x, y) = from
    if (x + distance > maxX && x - distance < 0 && y + distance > maxY && y - distance < 0)
      count
    else {
      val visibleAtDistance = Set.empty[(Int, Int)]
      val blocked = Set.empty[(Int, Int)]
      visibility(from, grid -- blocked, distance + 1, count + visibleAtDistance.size)
    }
  }

  override def answer1 = grid.map(asteroid => visibility(asteroid, grid)).max.toString

  override def answer2 = "unimplemented"
}
