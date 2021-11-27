package advent2020
import puzzleparse.{*, given}

object Day4:
  def part1(input: MultiLine[Map[String, String]]): MultiLine[Map[String, String]] =
    input.take(3).asInstanceOf[MultiLine[Map[String, String]]]

  def part2(input: String): String = input
