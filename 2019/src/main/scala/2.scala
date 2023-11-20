package day2
import parse.{*, given}
import year2019.IntCode

type I = Vector[Int] - ","

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    IntCode(input.updated(1, 12).updated(2, 2)).runSync(0)

  def part2(input: I): Int =
    val nounsAndVerbs = for
      noun <- (0 to 99).iterator
      verb <- (0 to 99).iterator
    yield (noun, verb)

    nounsAndVerbs
      .find((noun, verb) => IntCode(input.updated(1, noun).updated(2, verb)).runSync(0) == 19690720)
      .map((noun, verb) => 100 * noun + verb)
      .get
