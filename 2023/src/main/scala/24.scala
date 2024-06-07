package day24
import parse.{*, given}

case class Hailstone(position: (BigInt, BigInt, BigInt), velocity: (BigInt, BigInt, BigInt)):
  val (px, py, pz) = position
  val (vx, vy, vz) = velocity
  val a = vy
  val b = -vx
  val c = px*vy - py*vx

  val low  = BigInt("200000000000000")
  val high = BigInt("400000000000000")

  def inFuture(num: BigInt, denom: BigInt): Boolean =
    (num >= px * denom && vx*denom > 0) ||
    (num <= px * denom && vx*denom < 0)

  def inTestArea(num: BigInt, denom: BigInt): Boolean =
    if denom > 0 then
      num >= low*denom && num <= high*denom
    else if denom < 0 then
      num <= low*denom && num >= high*denom
    else
      false

  def xyIntersection(other: Hailstone): (BigInt, BigInt, BigInt) =
    val denom = a*other.b - other.a*b
    val xNum = other.b*c - b*other.c
    val yNum = a*other.c - other.a*c
    (xNum, yNum, denom)

  def xyPathIntersects(other: Hailstone): Boolean =
    val (xNum, yNum, denom) = xyIntersection(other)
    inTestArea(xNum, denom) &&
    inTestArea(yNum, denom) &&
    inFuture(xNum, denom) &&
    other.inFuture(xNum, denom)

given Read[(BigInt, BigInt, BigInt)] = Read[(BigInt, BigInt, BigInt)](",\\s+")
given Read[Hailstone] = Read("\\s+@\\s+")
given Read[List[Hailstone]] = Read("\n")

object Puzzle extends runner.Day[List[Hailstone], BigInt, BigInt]:
  def part1(hailstones: List[Hailstone]): BigInt =
    hailstones
      .combinations(2)
      .collect{case List(first, second) => first.xyPathIntersects(second)}
      .count(identity)

  def part2(hailstones: List[Hailstone]): BigInt =
    ???

  def plot(hailstones: List[Hailstone]): Unit =
    val points = hailstones
      .combinations(2)
      .collect{case List(first, second) => first.xyIntersection(second)}
      .collect{case (x, y, denom) if denom != 0 => (x.toDouble/denom.toDouble, y.toDouble/denom.toDouble)}
    visualizations.Plotly(points, title=Some("Advent of Code [2023 Day 24]"))
