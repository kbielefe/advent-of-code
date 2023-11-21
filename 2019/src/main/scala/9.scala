package day9
import cats.Eval
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val computer = IntCode[Eval](input)
    computer.input.put(1)
    computer.run
    computer.output.take

  def part2(input: I): Long =
    ???
