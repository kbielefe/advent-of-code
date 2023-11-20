package day5
import parse.{*, given}
import year2019.IntCode

type I = Vector[Int] - ","

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    val computer = IntCode(input)
    computer.input.put(1)
    computer.runSync
    Iterator.continually(computer.output.take).dropWhile(_ == 0).next

  def part2(input: I): Int =
    val computer = IntCode(input)
    computer.input.put(5)
    computer.runSync
    computer.output.take
