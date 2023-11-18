package day1
import parse.{*, given}

type I = List[List[Int] - "\n"] - "\n\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.map(_.sum).max

  def part2(input: I): Int =
    input.map(_.sum).sorted.reverse.take(3).sum
