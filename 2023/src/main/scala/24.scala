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
      .collect{case List(first, second) => first.xyPathIntersects(second)}
      .count(identity)

  def part2(hailstones: List[Hailstone]): BigInt =
    // px + t1*vx == h1.px + t1*h1.vx
    // py + t1*vy == h1.py + t1*h1.vy
    // pz + t1*vz == h1.pz + t1*h1.vz
    // px + t2*vx == h2.px + t2*h2.vx
    // py + t2*vy == h2.py + t2*h2.vy
    // pz + t2*vz == h2.pz + t2*h2.vz
    // px + t3*vx == h3.px + t3*h3.vx
    // py + t3*vy == h3.py + t3*h3.vy
    // pz + t3*vz == h3.pz + t3*h3.vz
    // 9 equations, 9 unknowns!
    // px py pz vx vy vz t1 t2 t3
    //
    // t1*vx - t1*h1.vx == h1.px - px
    // t1 == (h1.px - px)/(vx-h1.vx) == (h1.py - py)/(vy - h1.vy)
    // (h1.px - px)(vy - h1.vy) == (h1.py - py)(vx - h1.vx)
    hailstones.take(3).foreach(println)
    ???
