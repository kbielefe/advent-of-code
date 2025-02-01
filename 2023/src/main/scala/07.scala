package day7
import parse.{*, given}

enum HandType derives CanEqual:
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

case class Hand(cards: String, bid: Int):
  import HandType.*
  def part1HandType(cards: String): HandType =
    val sizes = cards.groupBy(identity).map(_._2.size).toList
    if sizes.contains(5) then
      FiveOfAKind
    else if sizes.contains(4) then
      FourOfAKind
    else if sizes.contains(3) && sizes.contains(2) then
      FullHouse
    else if sizes.contains(3) then
      ThreeOfAKind
    else if sizes.count(_ == 2) == 2 then
      TwoPair
    else if sizes.contains(2) then
      OnePair
    else
      HighCard

  def part2HandType: HandType =
    "23456789TQKA"
      .map(char => cards.replaceAll("J", char.toString))
      .map(part1HandType)
      .maxBy(_.ordinal)

  infix def part1lt(other: Hand): Boolean =
    val labels = "23456789TJQKA"
    val myHand = part1HandType(cards)
    val otherHand = part1HandType(other.cards)
    if myHand != otherHand then
      myHand.ordinal < otherHand.ordinal
    else
      val (l, r) = cards.zip(other.cards).dropWhile(_ == _).head
      labels.indexOf(l) < labels.indexOf(r)

  infix def part2lt(other: Hand): Boolean =
    val labels = "J23456789TQKA"
    if part2HandType != other.part2HandType then
      part2HandType.ordinal < other.part2HandType.ordinal
    else
      val (l, r) = cards.zip(other.cards).dropWhile(_ == _).head
      labels.indexOf(l) < labels.indexOf(r)

given Read[Hand] = Read("""(.+) (\d+)""".r)

type I = List[Hand]
given Read[I] = Read("\n")

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    given Ordering[Hand] = Ordering.fromLessThan(_ part1lt _)
    totalWinnings(input)

  def part2(input: I): Int =
    given Ordering[Hand] = Ordering.fromLessThan(_ part2lt _)
    totalWinnings(input)

  def totalWinnings(input: List[Hand])(using Ordering[Hand]): Int =
    input
      .sorted
      .zipWithIndex
      .map((hand, index) => hand.bid * (index + 1))
      .sum
