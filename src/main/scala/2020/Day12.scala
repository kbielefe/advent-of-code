package advent2020
import puzzleparse.{*, given}

object Day12:
  def part1(input: List[(Letter, Nat)]): Int =
    input.map(_._2).sum

  def part2(input: String): String = input
