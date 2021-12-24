package advent2021

object Day22:
  def part1(input: List[String]): Long =
    val cuboids = input.map(parseCuboid).takeWhile(_.x.start.abs <= 50)
    buildExpression(cuboids).cardinality.evaluate

  def part2(input: List[String]): Long =
    val cuboids = input.map(parseCuboid)
    buildExpression(cuboids).cardinality.evaluate

  def parseCuboid(input: String): Cuboid =
    val regex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r
    input match
      case regex(on, xmin, xmax, ymin, ymax, zmin, zmax) =>
        Cuboid(on == "on", xmin.toInt to xmax.toInt, ymin.toInt to ymax.toInt, zmin.toInt to zmax.toInt)

  def buildExpression(cuboids: List[Cuboid]): Expr =
    cuboids.foldLeft[Expr](Empty)((expr, cuboid) => if cuboid.on then Union(expr, cuboid) else Diff(expr, cuboid))

  sealed trait Expr derives CanEqual:
    def cardinality: Cardinality

  case object Empty extends Expr:
    def cardinality: Cardinality = Zero

  case class Diff(left: Expr, right: Cuboid) extends Expr:
    def cardinality: Cardinality =
      Minus(left.cardinality, Intersection(left, right).cardinality)

  case class Union(left: Expr, right: Cuboid) extends Expr:
    def cardinality: Cardinality =
      Minus(Plus(left.cardinality, right), Intersection(left, right).cardinality)

  case class Intersection(left: Expr, right: Cuboid) extends Expr:
    def cardinality: Cardinality = left match
      case Empty => Zero
      case Diff(dl, dr) =>
        Minus(Intersection(dl, right).cardinality, Intersection(Intersection(dl, dr), right).cardinality)
      case Union(ul, ur) => Union(Intersection(ul, right), ur & right).cardinality
      case Intersection(il, ir) => Intersection(il, ir & right).cardinality

  sealed trait Cardinality:
    def evaluate: Long

  case class Plus(left: Cardinality, right: Cardinality) extends Cardinality:
    def evaluate: Long = left.evaluate + right.evaluate

  case class Minus(left: Cardinality, right: Cardinality) extends Cardinality:
    def evaluate: Long = left.evaluate - right.evaluate

  case object Zero extends Cardinality:
    def evaluate: Long = 0

  case class Cuboid(on: Boolean, x: Range, y: Range, z: Range) extends Cardinality:
    override def toString: String =
      val onOrOff = if on then "on " else "off"
      f"$onOrOff $evaluate%3d ${x.start}%2d..${x.end}%2d, ${y.start}%2d..${y.end}%2d, ${z.start}%2d..${z.end}%2d"

    def evaluate: Long = x.length.toLong * y.length * z.length

    def &(other: Cuboid): Cuboid =
      val newX = x.overlap(other.x)
      val newY = y.overlap(other.y)
      val newZ = z.overlap(other.z)
      Cuboid(false, newX, newY, newZ)

  extension(r1: Range)
    def overlap(r2: Range): Range =
      Math.max(r1.start, r2.start) to Math.min(r1.end, r2.end)
