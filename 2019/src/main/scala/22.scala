package day22

import algorithms.*
import parse.{*, given}

sealed trait Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int]
  def matrix(size: Long): Matrix[2, 2, Long]

case object NewStack extends Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int] =
    deck.reverse
  def matrix(size: Long): Matrix[2, 2, Long] =
    Matrix(List(-1, size - 1), List(0, 1))

case class Increment(amount: Int) extends Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int] =
    deck.foldLeft((0, deck)){case ((position, result), card) => ((position + amount) % deck.size, result.updated(position, card))}._2
  def matrix(size: Long): Matrix[2, 2, Long] =
    Matrix(List(-amount, size * amount), List(0, 1))

case class Cut(amount: Int) extends Shuffle:
  def shuffle(deck: Vector[Int]): Vector[Int] =
    val splitPoint = if amount >= 0 then amount else deck.size + amount
    val (top, bottom) = deck.splitAt(splitPoint)
    bottom ++ top
  def matrix(size: Long): Matrix[2, 2, Long] =
    Matrix(List(1, size + amount), List(0, 1))

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
    //val shuffleCount = 101741582076661L
    //val size = 119315717514047L
    //val initial = Matrix.colVector[2, Long](2020, 1)
    //val shuffleCount = 1L
    //val size = 10007L
    //val initial = Matrix.colVector[2, Long](4086, 1)
    // Cut(-4) and Increment(3) are broken
    // Correct answer, but not doing modulus properly at the end
    val testCase = List(Cut(-4))
    val shuffleCount = 1L
    val size = 10L
    val initial = Matrix.colVector[2, Long](6, 1)
    val matrix = testCase.foldLeft(Matrix.identity[2, Long])((m, shuffle) => m * shuffle.matrix(size))
    val result = matrix.powMod(shuffleCount, size) * initial
    println(result.element[0, 0])
    // Expected 2019
    ???
