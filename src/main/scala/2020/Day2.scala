package advent2020
import puzzleparse.{Letter, Letters}

object Day2:
  def part1(input: List[(Int, Int, Letter, Letters)]): String =
    input.head._4.drop(10)

  def part2(input: String): String = input
