package day9
import parse.{*, given}
import year2019.IntCode

type I = Vector[Long] - ","

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val computer = IntCode(input)
    computer.input.put(1)
    computer.runSync
    computer.output.take

  def part2(input: I): Long =
    ???
