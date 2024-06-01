package day22
import algorithms.{Grid, Matrix}
import algorithms.Grid.{*, given}
import parse.{*, given}
import scala.annotation.tailrec
import scala.util.Random

case class Point(x: Int, y: Int, z: Int):
  def withOffset(offset: Int): Point =
    Point(x, y, z - offset)

case class Brick(start: Point - ",", end: Point - ",") derives CanEqual:
  def points: Set[Point] = for
    x <- (start.x to end.x).toSet
    y <- (start.y to end.y).toSet
    z <- (start.z to end.z).toSet
  yield Point(x, y, z)

  def minZ: Int = Math.min(start.z, end.z)

  def settle(heights: Map[(Int, Int), Set[Int]]): Brick =
    val offset = points.map(point => point.z - heights.getOrElse((point.x, point.y), List(0)).filter(_ < point.z).max - 1).min
    Brick(start.withOffset(offset).asInstanceOf[Point - ","], end.withOffset(offset).asInstanceOf[Point - ","])

  def supportedBy(brick: Brick): Boolean =
    brick != this && (brick.points.map(_.withOffset(-1)) & points).size >= 1

  def canDisintegrate(bricks: Set[Brick]): Boolean =
    val above = bricks.filter(_.supportedBy(this))
    val supporting = above.flatMap(top => bricks.filter(bottom => top.supportedBy(bottom)))
    above.isEmpty || supporting.size > 1

end Brick

type I = Set[Brick - "~"] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(bricks: I): Long =
    val settled = settle(bricks.asInstanceOf[Set[Brick]])
    println(bricksToString(settled))
    //settled.count(_.canDisintegrate(settled))
    ???

  def part2(bricks: I): Long =
    ???

  def bricksToString(bricks: Set[Brick]): String =
    val random = Random(0)
    val points = bricks.flatMap{brick =>
      val char: Char = random.between('a', 'z' + 1).toChar
      brick.points.map(point => Pos(point.z, point.x) -> char) ++
      brick.points.map(point => Pos(point.z, point.y + 11) -> char)
    }.toMap
    val grid = Grid(points).transform(Matrix.reflectX)
    grid.toString

  @tailrec
  def settle(unsettled: Set[Brick], settled: Set[Brick] = Set.empty): Set[Brick] =
    if unsettled.isEmpty then
      settled
    else
      val minZ = unsettled.map(_.minZ).min
      val (newSettled, stillUnsettled) = unsettled.partition(_.minZ == minZ)
      val heights = settled.flatMap(_.points).groupMap(point => (point.x, point.y))(_.z)
      val movedDown = newSettled.map(_.settle(heights))
      settle(stillUnsettled, settled ++ movedDown)
