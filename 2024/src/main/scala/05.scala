package day5
import parse.{*, given}

case class Input(rules: Set[String], updates: List[Vector[Int]]):
  val ordering: Ordering[Int] = Ordering.fromLessThan((l, r) => rules.contains(s"$l|$r"))

given Read[Set[String]] = Read("\n")
given Read[Vector[Int]] = Read(",")
given Read[List[Vector[Int]]] = Read("\n")
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Int, Int]:
  def part1(input: Input): Int =
    given Ordering[Int] = input.ordering
    input.updates
      .filter(update => update == update.sorted)
      .map(middle)
      .sum

  def part2(input: Input): Int =
    given Ordering[Int] = input.ordering
    input.updates
      .filterNot(update => update == update.sorted)
      .map(_.sorted)
      .map(middle)
      .sum

  def middle(update: Vector[Int]): Int =
    update(update.size / 2)
