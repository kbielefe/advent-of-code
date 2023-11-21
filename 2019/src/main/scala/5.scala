package day5
import cats.Eval
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val computer = IntCode[Eval](input)
    computer.input.put(1)
    computer.run
    Iterator.continually(computer.output.take).dropWhile(_ == 0).next

  def part2(input: I): Long =
    val computer = IntCode[Eval](input)
    computer.input.put(5)
    computer.run
    computer.output.take
