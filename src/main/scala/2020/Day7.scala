package advent2020
import puzzleparse.{*, given}

object Day7:
  def part1(input: DMap["contain", String, DList[", ", String]]): List[String] =
    input.head._2

  def part2(input: String): String = input
