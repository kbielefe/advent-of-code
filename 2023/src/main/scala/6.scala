package day6
import parse.{*, given}

type I = (List[Long] - "\\s+", List[Long] - "\\s+") ~ """Time:\s+(.+)\nDistance:\s+(.+)"""

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val (times, distances) = input
    times.map(_.toDouble).zip(distances.map(_.toDouble)).map(winCount.tupled).product

  def part2(input: I): Long =
    val (times, distances) = input
    val time = times.map(_.toString).mkString.toDouble
    val distance = distances.map(_.toString).mkString.toDouble
    winCount(time, distance)

  def winCount(raceDuration: Double, distanceToBeat: Double): Long =
    val x = math.sqrt(raceDuration * raceDuration - 4.0 * distanceToBeat)
    val z1 = (raceDuration + x) / 2.0
    val z2 = (raceDuration - x) / 2.0
    math.abs(z1 - z2).toLong
