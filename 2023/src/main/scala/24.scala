package day24

import algorithms.breeze.*
import algorithms.spire.given
import breeze.linalg.{*, given}
import breeze.numerics.{*, given}
import io.circe.Encoder
import parse.{*, given}
import spire.math.{Rational, SafeLong}

case class Hailstone(position: (Rational, Rational, Rational), velocity: (Rational, Rational, Rational)):
  val (px, py, pz) = position
  val (vx, vy, vz) = velocity
  val a = vy
  val b = -vx
  val c = px*vy - py*vx

  val low  = Rational("200000000000000")
  val high = Rational("400000000000000")

  def inFuture(x: Rational): Boolean =
    (x >= px && vx > 0) ||
    (x <= px && vx < 0)

  def inTestArea(num: Rational): Boolean =
    num >= low && num <= high

  def xyIntersection(other: Hailstone): Option[(Rational, Rational)] =
    val denom = a*other.b - other.a*b
    Option.when(denom != 0)((other.b*c - b*other.c) / denom, (a*other.c - other.a*c) / denom)

  def xyPathIntersects(other: Hailstone): Boolean =
    xyIntersection(other) match
      case None => false
      case Some((x, y)) =>
        inTestArea(x) &&
        inTestArea(y) &&
        inFuture(x) &&
        other.inFuture(x)

given Read[(Rational, Rational, Rational)] = Read[(Rational, Rational, Rational)](",\\s+")
given Read[Hailstone] = Read("\\s+@\\s+")
given Read[List[Hailstone]] = Read("\n")

object Puzzle extends runner.Day[List[Hailstone], Int, Long]:
  def part1(hailstones: List[Hailstone]): Int =
    hailstones
      .combinations(2)
      .collect{case List(first, second) => first.xyPathIntersects(second)}
      .count(identity)

  def part2(hailstones: List[Hailstone]): Long =
    val vx = velocity(hailstones, _.vx, _.px)
    val vy = velocity(hailstones, _.vy, _.py)
    val vz = velocity(hailstones, _.vz, _.pz)
    val h1 :: h2 :: _ = hailstones: @unchecked
    val a = DenseMatrix(
      (1.0, 0.0, 0.0, (vx - h1.vx).toDouble, 0.0),
      (0.0, 1.0, 0.0, (vy - h1.vy).toDouble, 0.0),
      (0.0, 0.0, 1.0, (vz - h1.vz).toDouble, 0.0),
      (1.0, 0.0, 0.0, 0.0, (vx - h2.vx).toDouble),
      (0.0, 1.0, 0.0, 0.0, (vy - h2.vy).toDouble)
    )
    val r = DenseVector(h1.px.toDouble, h1.py.toDouble, h1.pz.toDouble, h2.px.toDouble, h2.py.toDouble)
    val result = a \ r
    result(0).toLong + result(1).toLong + result(2).toLong

  def velocity(hailstones: List[Hailstone], v: Hailstone => Rational, p: Hailstone => Rational): Rational =
    val possibilities = hailstones
      .groupBy(v)
      .filter(_._2.size > 1)
      .values
      .flatMap(_.combinations(2))
      .collect{case List(h1, h2) if p(h1) != p(h2) =>
        val primeFactors = (p(h1) - p(h2))
          .abs
          .toSafeLong
          .factor
          .toList
          .flatMap{case (f, count) => List.fill(count)(f)}
        allFactors(primeFactors).flatMap(factor => Set(v(h1) + factor, v(h1) - factor))
      }
      .reduceLeft(_ & _)
    assert(possibilities.size == 1, "detected more than one possible velocity")
    possibilities.head

  def allFactors(primeFactors: List[SafeLong]): Set[SafeLong] = primeFactors match
    case Nil => Set(1)
    case head :: tail =>
      val tailFactors = allFactors(tail)
      tailFactors ++ tailFactors.map(_ * head)
