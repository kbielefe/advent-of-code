package advent2022
import puzzleparse.{*, given}

object Day1:
  def part1(input: MultiLine[List[Int]]): Int =
    input.map(_.sum).max

  def part2(input: String): Int =
    input.split("\n\n").map(_.split("\n").map(_.toInt)).map(_.sum).sorted.reverse.take(3).sum
