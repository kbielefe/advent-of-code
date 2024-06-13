package day24

import algorithms.Spire.given
import io.circe.Encoder
import parse.{*, given}
import spire.math.Rational

case class Hailstone(position: (Rational, Rational, Rational), velocity: (Rational, Rational, Rational)):
  val (px, py, pz) = position
  val (vx, vy, vz) = velocity
  val a = vy
  val b = -vx
  val c = px*vy - py*vx

  val low  = Rational("200000000000000")
  val high = Rational("400000000000000")

  def inFuture(num: Rational): Boolean =
    (num >= px && vx > 0) ||
    (num <= px && vx < 0)

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

object Puzzle extends runner.Day[List[Hailstone], Int, Rational]:
  def part1(hailstones: List[Hailstone]): Int =
    hailstones
      .combinations(2)
      .collect{case List(first, second) => first.xyPathIntersects(second)}
      .count(identity)

  def part2(hailstones: List[Hailstone]): Rational =
    ???
