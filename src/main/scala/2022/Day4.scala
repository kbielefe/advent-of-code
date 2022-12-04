package advent2022
import puzzleparse.{*, given}

object Day4:
  case class Pair(a: Nat, b: Nat, c: Nat, d: Nat):
    def encloses: Boolean =
      (a <= c && b >= d) || (c <= a && d >= b)
    def overlaps: Boolean =
      (a <= c && b >= c) ||
      (a <= d && b >= d) ||
      (c <= a && d >= a) ||
      (c <= b && d >= b)
  end Pair

  def part1(input: List[Pair]): Int =
    input.count(_.encloses)

  def part2(input: List[Pair]): Int =
    input.count(_.overlaps)
