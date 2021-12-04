package advent2020
import puzzleparse.{*, given}

object Day3:
  def part1(input: Grid[Char]): Char =
    input((0, 1))

  def part2(input: String): String = input
