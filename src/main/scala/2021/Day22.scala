package advent2021

object Day22:
  def part1(input: List[String]): Long =
    val cuboids = input.map(parseCuboid).takeWhile(_.x.start.abs <= 50)
    cuboids.tails.map(tail => buildExpression(tail).cardinality.evaluate).sum

  def part2(input: List[String]): Long =
    val cuboids = input.map(parseCuboid)
    cuboids.tails.map(tail => buildExpression(tail).cardinality.evaluate).sum

  def parseCuboid(input: String): Cuboid =
    val regex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r
    input match
      case regex(on, xmin, xmax, ymin, ymax, zmin, zmax) =>
        Cuboid(on == "on", xmin.toInt to xmax.toInt, ymin.toInt to ymax.toInt, zmin.toInt to zmax.toInt)

  def buildExpression(cuboids: List[Cuboid]): Expr = cuboids match
    case head :: tail if head.on =>
      tail.filter(_.overlaps(head)).foldLeft[Expr](head)((expr, cuboid) => Diff(expr, cuboid))
    case _ => Empty

  sealed trait Expr derives CanEqual:
    def cardinality: Cardinality

  case object Empty extends Expr:
    def cardinality: Cardinality = Zero

  case class Diff(left: Expr, right: Cuboid) extends Expr:
    def cardinality: Cardinality =
      Minus(left.cardinality, Intersection(left, right).cardinality)

  case class Intersection(left: Expr, right: Cuboid) extends Expr:
    def cardinality: Cardinality = left match
      case Empty =>
        Zero
      case c: Cuboid =>
        c & right
      case Intersection(il, ir) =>
        Intersection(il, ir & right).cardinality
      case Diff(dl, dr) =>
        Minus(Intersection(dl, right).cardinality, Intersection(Intersection(dl, dr), right).cardinality)

  sealed trait Cardinality:
    def evaluate: Long

  case class Minus(left: Cardinality, right: Cardinality) extends Cardinality:
    def evaluate: Long = left.evaluate - right.evaluate

  case object Zero extends Cardinality:
    def evaluate: Long = 0

  case class Cuboid(on: Boolean, x: Range, y: Range, z: Range) extends Expr with Cardinality:
    def cardinality: Cardinality = this
    def evaluate: Long = x.length.toLong * y.length * z.length

    def overlaps(other: Cuboid): Boolean =
      val newX = x.overlap(other.x)
      val newY = y.overlap(other.y)
      val newZ = z.overlap(other.z)
      !(newX.isEmpty || newY.isEmpty || newZ.isEmpty)

    def &(other: Cuboid): Cuboid =
      val newX = x.overlap(other.x)
      val newY = y.overlap(other.y)
      val newZ = z.overlap(other.z)
      Cuboid(false, newX, newY, newZ)

  extension(r1: Range)
    def overlap(r2: Range): Range =
      Math.max(r1.start, r2.start) to Math.min(r1.end, r2.end)
