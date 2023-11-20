package day17
import parse.given
import scala.annotation.tailrec
import algorithms.{detectCycle, cycledEquivalentValue}

object Puzzle extends runner.Day[String, Int, Long]:
  def part1(input: String): Int =
    moves(input).map(_._1).drop(2022).next

  def part2(input: String): Long =
    val rockIndex = Iterator.continually((1 to 5).iterator).flatten
    val jetIndex = Iterator.continually((1 to input.trim.size).iterator).flatten
    val skylines = rockIndex.zip(jetIndex).zip(moves(input)).map(skyline)
    val Some((start, period, repeated)) = detectCycle(skylines): @unchecked
    cycledEquivalentValue(start, period, 1000000000000, n => moves(input).map(_._1).drop(n.toInt).next.toLong)

  private def skyline(state: ((Int, Int), (Int, Set[(Int, Int)]))): (Int, Int, List[Int]) =
    val ((rockIndex, jetIndex), (height, cubes)) = state
    val highest = (1 to 7).toList.map{x =>
      val cubesInColumn = cubes.filter(_._1 == x).map(_._2)
      val max = if cubesInColumn.isEmpty then 0 else cubesInColumn.max
      height - max
    }
    (rockIndex, jetIndex, highest)

  private def moves(input: String): Iterator[(Int, Set[(Int, Int)])] =
    val rocks = Iterator.continually(rockOrder.iterator).flatten
    val jets = Iterator.continually(input.trim.iterator).flatten
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
    }

  private def printRocks(height: Int, rocks: Set[(Int, Int)]): Unit =
    val grid = (height to 1 by -1).map{y =>
      val row = (1 to 7).map{x =>
        if rocks.contains((x, y)) then '#' else '.'
      }.mkString
      "|" + row + "|"
    }.mkString("\n")
    println(grid + "\n|1234567|\n\n")

  val rockOrder = List(
    Set((0, 0), (1, 0), (2, 0), (3, 0)),
    Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)),
    Set((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)),
    Set((0, 0), (0, 1), (0, 2), (0, 3)),
    Set((0, 0), (0, 1), (1, 0), (1, 1))
  )

