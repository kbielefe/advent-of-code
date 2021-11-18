package advent2020
import puzzleparse.{*, given}

object Day2:
  case class Password(min: Int, max: Int, letter: Letter, password: Letters) derives Read

  def part1(input: List[Password]): List[String] =
    input.map(_.password).take(10)

  def part2(input: String): String = input
