package day2
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    IntCode(input.updated(1, 12L).updated(2, 2L)).runSync(0)

  def part2(input: I): Long =
    val nounsAndVerbs = for
      noun <- (0 to 99).iterator
      verb <- (0 to 99).iterator
    yield (noun, verb)

    nounsAndVerbs
      .find((noun, verb) => IntCode(input.updated(1, noun.toLong).updated(2, verb.toLong)).runSync(0) == 19690720)
      .map((noun, verb) => 100 * noun + verb)
      .get
