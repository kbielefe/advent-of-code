package advent2020
import puzzleparse.{*, given}

object Day8:
  def part1(input: List[(Letters, Int)]): List[(String, Int)] =
    input.take(10)

  def part2(input: String): String = input
