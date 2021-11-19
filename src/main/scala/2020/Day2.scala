package advent2020
import puzzleparse.{*, given}

object Day2:
  case class Password(min: Int, max: Int, letter: Letter, password: Letters) derives Read, Show

  def part1(input: List[Password]): List[Password] =
    input.take(10)

  def part2(input: String): String = input
