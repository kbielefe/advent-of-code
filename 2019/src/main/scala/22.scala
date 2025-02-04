package day22

import algorithms.{*, given}, breeze.*, Mod.given
import _root_.breeze.linalg.{Vector as BreezeVector, *, given}
import parse.{*, given}
import math.Integral.Implicits.infixIntegralOps
import scala.language.implicitConversions

sealed trait Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long]
  def matrix(using Modulus[Long]): DenseMatrix[Mod[Long]]

case object NewStack extends Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long] =
    deck.reverse
  def matrix(using Modulus[Long]): DenseMatrix[Mod[Long]] =
    DenseMatrix((Mod(-1L), Mod(-1L)), (Mod(0L), Mod(1L)))

case class Increment(amount: Long) extends Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long] =
    deck.foldLeft((0L, deck)){case ((position, result), card) => ((position + amount) % deck.size.toLong, result.updated(position.toInt, card))}._2
  def matrix(using Modulus[Long]): DenseMatrix[Mod[Long]] =
    DenseMatrix((Mod(Mod.modInverse(amount)), Mod(0L)), (Mod(0L), Mod(1L)))

case class Cut(amount: Long) extends Shuffle:
  def shuffle(deck: Vector[Long]): Vector[Long] =
    val splitPoint = if amount >= 0 then amount.toInt else deck.size + amount.toInt
    val (top, bottom) = deck.splitAt(splitPoint)
    bottom ++ top
  def matrix(using Modulus[Long]): DenseMatrix[Mod[Long]] =
    DenseMatrix((Mod(1L), Mod(amount)), (Mod(0L), Mod(1L)))

given Read[Shuffle] with
  def read(input: String): Shuffle = input match
    case "deal into new stack"     => NewStack
    case s"deal with increment $a" => Increment(a.toLong)
    case s"cut $a"                 => Cut(a.toLong)

type I = List[Shuffle]
given Read[I] = Read("\n")

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val deck = input.foldLeft(Vector.range(0L, 10007L))((deck, shuffle) => shuffle.shuffle(deck))
    deck.indexWhere(_ == 2019)

  def part2(input: I): Long =
    given Modulus[Long] = Modulus(119315717514047L)
    val shuffleCount = 101741582076661L
    val initial = DenseVector[Mod[Long]](Mod(2020L), Mod(1L))
    val matrix = input.map(_.matrix).foldLeft(DenseMatrix.eye[Mod[Long]](2))(_ * _)
    val result = (matrix ^ shuffleCount) * initial
    result(0).value
