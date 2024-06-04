package day22
import algorithms.*
import algorithms.Grid.Pos
import parse.{*, given}

case class Point(x: Int, y: Int, z: Int):
  def -(amount: Int): Point =
    Point(x, y, z - amount)

case class Brick(start: Point, end: Point):
  def minZ: Int =
    Math.min(start.z, end.z)

  def points: Seq[Point] = for
    x <- start.x to end.x
    y <- start.y to end.y
    z <- start.z to end.z
  yield Point(x, y, z)

  def -(amount: Int): Brick =
    Brick(start - amount, end - amount)

given Read[Point] = Read(",")
given Read[Brick] = Read("~")
given Read[List[Brick]] = Read("\n")

object Puzzle extends runner.Day[List[Brick], Int, Int]:
  def part1(bricks: List[Brick]): Int =
    val settled = settle(bricks)
    ???

  def part2(bricks: List[Brick]): Int =
    ???

  private def settle(bricks: List[Brick]): Set[Brick] =
    bricks.sortBy(_.minZ).foldLeft(Map.empty[Pos, Int] -> Set.empty[Brick]){case ((skyline, result), brick) =>
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
