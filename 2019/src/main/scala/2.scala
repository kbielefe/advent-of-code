package day2
import parse.{*, given}
import year2019.IntCode

type I = Vector[Int] - ","

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    IntCode(input.updated(1, 12).updated(2, 2)).runSync(0)

  def part2(input: I): Int =
    ???
