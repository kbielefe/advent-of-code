package advent2021
import puzzleparse.{*, given}

object Day7:
  def part1(input: DList[",", Int]): Int =
    (input.min to input.max).map(pos => input.map(x => (x - pos).abs).sum).min

  def part2(input: DList[",", Int]): Int =
    def fuel(n: Int): Int = n * (n + 1) / 2
    (input.min to input.max).map(pos => input.map(x => fuel((x - pos).abs)).sum).min
