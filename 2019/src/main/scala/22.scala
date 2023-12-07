package day22

import algorithms.*
import parse.{*, given}

sealed trait Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int]
  val deckSize = 119315717514047L

case object NewStack extends Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int] =
    deck.reverse

case class Increment(amount: Int) extends Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int] =
    deck.foldLeft((0, deck)){case ((position, result), card) => ((position + amount) % deck.size, result.updated(position, card))}._2
  val inverse = BigInt(Modular.multiplicativeInverse(amount.toLong, deckSize).get)
  val bigIntMod = BigInt(deckSize)

case class Cut(amount: Int) extends Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int] =
    val splitPoint = if amount >= 0 then amount else deck.size + amount
    val (top, bottom) = deck.splitAt(splitPoint)
    bottom ++ top

given Read[Shuffle] with
  def read(input: String): Shuffle = input match
    case "deal into new stack"     => NewStack
    case s"deal with increment $a" => Increment(a.toInt)
    case s"cut $a"                 => Cut(a.toInt)

type I = List[Shuffle] - "\n"

object Puzzle extends runner.Day[I, Int, Long]:
  def part1(input: I): Int =
    val deck = input.foldLeft(Vector.range(0, 10007))((deck, shuffle) => shuffle.shuffle(deck))
    deck.indexWhere(_ == 2019)

  def part2(input: I): Long =
    ???
