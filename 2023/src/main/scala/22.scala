package day22
import algorithms.*
import algorithms.Grid.Pos
import parse.{*, given}

case class Point(x: Int, y: Int, z: Int):
  def -(amount: Int): Point =
    Point(x, y, z - amount)

  def +(amount: Int): Point =
    Point(x, y, z + amount)

case class Brick(start: Point, end: Point) derives CanEqual:
  def minZ: Int =
    Math.min(start.z, end.z)

  def points: Set[Point] = for
    x <- (start.x to end.x).toSet
    y <- start.y to end.y
    z <- start.z to end.z
  yield Point(x, y, z)

  def -(amount: Int): Brick =
    Brick(start - amount, end - amount)

case class Bricks(bricks: Set[Brick]):
  private lazy val byPoint =
    bricks
      .flatMap(brick => brick.points.map(point => point -> brick))
      .toMap

  extension (brick: Brick)
    def above: Set[Brick] =
      brick.points.flatMap(point => byPoint.get(point + 1)) - brick

    def below: Set[Brick] =
      brick.points.flatMap(point => byPoint.get(point - 1)) - brick

    def safeToDisintegrate: Boolean =
      brick.above.isEmpty || brick.above.forall(_.below.exists(_ != brick))

  def safeToDisintegrate: Int =
    bricks.count(_.safeToDisintegrate)

  def settle: Bricks =
    val settled = bricks.toList.sortBy(_.minZ).foldLeft(Map.empty[Pos, Int] -> Set.empty[Brick]){case ((skyline, result), brick) =>
      val moveDownAmount = brick.points.map{point =>
        val pos = Pos(point.x, point.y)
        val height = skyline.getOrElse(pos, 0)
        point.z - height - 1
      }.min
      val newBrick = brick - moveDownAmount
      val newSkyline = newBrick.points.toList.sortBy(_.z).foldLeft(skyline)((skyline, point) =>
        skyline.updated(Pos(point.x, point.y), point.z)
      )
      (newSkyline, result + newBrick)
    }._2
    new Bricks(settled)

given Read[Point] = Read(",")
given Read[Brick] = Read("~")
given Read[Set[Brick]] = Read("\n")
given Read[Bricks] = Read()

object Puzzle extends runner.Day[Bricks, Int, Int]:
  def part1(bricks: Bricks): Int =
    bricks.settle.safeToDisintegrate

  def part2(bricks: Bricks): Int =
    ???
