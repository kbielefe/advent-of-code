package day4
import parse.{*, given}

case class Card(number: Int, winningNumbers: Set[Int], myNumbers: Set[Int]):
  def matchingNumberCount: Int = (myNumbers & winningNumbers).size
  def points: Int = Math.pow(2, matchingNumberCount - 1).toInt

type I = List[Card]
given Read[Set[Int]] = Read("\\s+")
given Read[Card] = Read("""Card\s+(\d+):\s+(.+)\s+\|\s+(.+)""".r)
given Read[I] = Read("\n")

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.map(_.points).sum

  def part2(input: I): Int =
    val initialCounts = (1 to input.size).map(number => (number -> 1)).toMap
    input.foldLeft(initialCounts){(counts, card) =>
      (1 to card.matchingNumberCount)
        .map(_ + card.number)
        .filter(_ <= input.size)
        .foldLeft(counts)((counts, copy) => counts.updated(copy, counts(copy) + counts(card.number)))
    }.map(_._2).sum
