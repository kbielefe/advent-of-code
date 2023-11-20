package day17
import parse.given
import scala.annotation.tailrec
import algorithms.{detectCycle, cycledEquivalentValue}

object Puzzle extends runner.Day[String, Int, Long]:
  def part1(input: String): Int =
    heights(input).drop(2022).next

  def part2(input: String): Long =
    def heightDiffs = heights(input)
      .sliding(2)
      .map{case Seq(x, y) => y - x}
    val Some((start, period)) = detectCycle(heightDiffs): @unchecked
    cycledEquivalentValue(start, period, 1000000000000, n => heights(input).drop(n).next)

  private def heights(input: String): Iterator[Int] =
    val rocks = Iterator.continually(rockOrder.iterator).flatten
    val jets = Iterator.continually(input.iterator).flatten
    rocks.scanLeft((0, Set.empty[(Int, Int)])){case ((height, cubes), rock) =>
      @tailrec
      def moveUntilBlocked(rocks: Set[(Int, Int)]): (Int, Set[(Int, Int)]) =
        val movedRocks = if jets.next == '>' then
          rocks.map{case (x, y) => (x + 1, y)}
        else
          rocks.map{case (x, y) => (x - 1, y)}
        val movedVertically = if movedRocks.map(_._1).exists(x => x < 1 || x > 7) || movedRocks.exists(cubes.contains) then
          rocks
        else
          movedRocks
        val movedDownRocks = movedVertically.map{case (x, y) => (x, y - 1)}
        if movedDownRocks.map(_._2).exists(_ <= 0) || movedDownRocks.exists(cubes.contains) then
          val newHeight = Math.max(movedVertically.map(_._2).max, height)
          val newCubes = cubes.filter{case (x, y) => y >= height - 100} ++ movedVertically
          (newHeight, newCubes)
        else
          moveUntilBlocked(movedDownRocks)
      val initialRockCubes = rock.map{case (x, y) => (x + 3, y + height + 4)}
      moveUntilBlocked(initialRockCubes)
    }.map(_._1)

  val rockOrder = List(
    Set((0, 0), (1, 0), (2, 0), (3, 0)),
    Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)),
    Set((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)),
    Set((0, 0), (0, 1), (0, 2), (0, 3)),
    Set((0, 0), (0, 1), (1, 0), (1, 1))
  )
