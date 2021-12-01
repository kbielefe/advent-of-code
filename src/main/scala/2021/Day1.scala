package advent2021
import puzzleparse.{*, given}

object Day1:
  def part1(input: List[Int]): Int =
    input
      .sliding(2)
      .count{case Seq(x, y) => y > x}

  def part2(input: List[Int]): Int =
    input
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count{case Seq(x, y) => y > x}
