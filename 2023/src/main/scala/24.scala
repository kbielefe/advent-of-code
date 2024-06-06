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

  def xyPathIntersects(other: Hailstone): Boolean =
    val denom = a*other.b - other.a*b
    val xNum = other.b*c - b*other.c
    val yNum = a*other.c - other.a*c
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
      .count{case List(first, second) => first.xyPathIntersects(second)}

  def part2(hailstones: List[Hailstone]): BigInt =
    ???
