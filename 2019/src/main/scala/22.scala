package day22

import algorithms.{*, given}
import parse.{*, given}
import math.Integral.Implicits.infixIntegralOps
import scala.language.implicitConversions

sealed trait Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long]
  def matrix(size: Long)(using Modulus[Long]): Matrix[2, 2, Mod[Long]]

case object NewStack extends Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long] =
    deck.reverse
  def matrix(size: Long)(using Modulus[Long]): Matrix[2, 2, Mod[Long]] =
    Matrix(List(-1, size - 1).map(Mod.apply), List(0, 1).map(Mod.apply))

case class Increment(amount: Long) extends Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long] =
    deck.foldLeft((0L, deck)){case ((position, result), card) => ((position + amount) % deck.size.toLong, result.updated(position.toInt, card))}._2
  def matrix(size: Long)(using Modulus[Long]): Matrix[2, 2, Mod[Long]] =
    Matrix(List(-amount, size * amount).map(Mod.apply), List(0, 1).map(Mod.apply))

case class Cut(amount: Long) extends Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long] =
    val splitPoint = if amount >= 0 then amount.toInt else deck.size + amount.toInt
    val (top, bottom) = deck.splitAt(splitPoint)
    bottom ++ top
  def matrix(size: Long)(using Modulus[Long]): Matrix[2, 2, Mod[Long]] =
    Matrix(List(1, size + amount).map(Mod.apply), List(0, 1).map(Mod.apply))

given Read[Shuffle] with
  def read(input: String): Shuffle = input match
    case "deal into new stack"     => NewStack
    case s"deal with increment $a" => Increment(a.toLong)
    case s"cut $a"                 => Cut(a.toLong)

type I = List[Shuffle] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val deck = input.foldLeft(Vector.range(0L, 10007L))((deck, shuffle) => shuffle.shuffle(deck))
    deck.indexWhere(_ == 2019)

  def part2(input: I): Long =
    //val shuffleCount = 101741582076661L
    //val size = 119315717514047L
    //val initial = Matrix.colVector[2, Long](2020, 1)
    val size = 10007L
    given Modulus[Long] = Modulus(size)
    val shuffleCount = Mod(1L)
    val initial = Matrix.colVector[2, Mod[Long]](4086, 1)
    val matrix = input.reverse.map(_.matrix(size)).foldLeft(Matrix.identity[2, Mod[Long]])(_ * _)
    val result = (matrix ^ shuffleCount) * initial
    println(initial)
    println(matrix)
    println(result)
    println(result.element[0, 0] + Mod(size))
    // Expected 2019
    ???
