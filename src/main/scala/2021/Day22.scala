package advent2021

object Day22:
  def part1(input: List[String]): Long =
    val cuboids = input.map(parseCuboid).takeWhile(_.x.start.abs <= 50)
    val all = overlapAll(cuboids)
    println(all)
    all.set.size

  def part2(input: List[String]): Long =
    ???

  def parseCuboid(input: String): Cuboid =
    val regex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r
    input match
      case regex(on, xmin, xmax, ymin, ymax, zmin, zmax) =>
        Cuboid(
          on == "on",
          xmin.toInt to xmax.toInt,
          ymin.toInt to ymax.toInt,
          zmin.toInt to zmax.toInt
        )

  def overlapAll(cuboids: List[Cuboid]): CubeSet =
    cuboids.foldLeft[CubeSet](Empty)((result, next) => if next.on then result | next else result -- next)

  sealed trait CubeSet:
    def |(other: CubeSet): CubeSet = Union(this, other)
    def --(other: CubeSet): CubeSet = Diff(this, other)
    def &(other: CubeSet): CubeSet = Intersection(this, other)
    def set: Set[(Int, Int, Int)]

  case object Empty extends CubeSet:
    def set: Set[(Int, Int, Int)] = Set.empty

  case class Intersection(left: CubeSet, right: CubeSet) extends CubeSet:
    def set: Set[(Int, Int, Int)] = left.set & right.set

  case class Union(left: CubeSet, right: CubeSet) extends CubeSet:
    def set: Set[(Int, Int, Int)] = left.set | right.set

  case class Diff(left: CubeSet, right: CubeSet) extends CubeSet:
    def set: Set[(Int, Int, Int)] = left.set -- right.set

  case class Cuboid(on: Boolean, x: Range, y: Range, z: Range) extends CubeSet:
    def set: Set[(Int, Int, Int)] = for
      xs <- x.toSet
      ys <- y.toSet
      zs <- z.toSet
    yield (xs, ys, zs)
