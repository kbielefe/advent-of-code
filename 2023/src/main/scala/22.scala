package day22
import algorithms.*
import algorithms.Grid.{*, given}
import parse.{*, given}
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

case class Point(x: Int, y: Int, z: Int) derives CanEqual:
  def -(amount: Int): Point =
    Point(x, y, z - amount)

  def +(amount: Int): Point =
    Point(x, y, z + amount)

case class Brick(start: Point - ",", end: Point - ",") derives CanEqual:
  def points: Set[Point] = for
    x <- (start.x to end.x).toSet
    y <- (start.y to end.y).toSet
    z <- (start.z to end.z).toSet
  yield Point(x, y, z)

  def minZ: Int =
    Math.min(start.z, end.z)

  def -(amount: Int): Brick =
    Brick((start - amount).asInstanceOf[Point - ","], (end - amount).asInstanceOf[Point - ","])

  def above(bricksByPoint: Map[Point, Brick]): Set[Brick] =
    points.flatMap(point => bricksByPoint.get(point + 1)) - this

  def below(bricksByPoint: Map[Point, Brick]): Set[Brick] =
    points.flatMap(point => bricksByPoint.get(point - 1)) - this

  def safeToDisintegrate(bricksByPoint: Map[Point, Brick]): Boolean =
    above(bricksByPoint).isEmpty || above(bricksByPoint).forall(_.below(bricksByPoint).exists(_ != this))

end Brick

type I = Set[Brick - "~"] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(bricks: I): Long =
    val settled = settle(bricks.asInstanceOf[Set[Brick]])
    val bricksByPoint = settled.flatMap(brick => brick.points.map(point => point -> brick)).toMap
    settled.count(_.safeToDisintegrate(bricksByPoint))

  def part2(bricks: I): Long =
    val settled = settle(bricks.asInstanceOf[Set[Brick]])
    val bricksByPoint = settled.flatMap(brick => brick.points.map(point => point -> brick)).toMap
    settled.map(brick => chainReaction(bricksByPoint, Queue(brick))).sum

  @tailrec
  def chainReaction(
    bricksByPoint: Map[Point, Brick],
    toVisit: Queue[Brick],
    disintegrated: Set[Brick] = Set.empty,
    queued: Set[Brick] = Set.empty
  ): Int =
    toVisit.dequeueOption match
      case Some((brick, remaining)) if brick.below(bricksByPoint).isEmpty || queued.isEmpty =>
        chainReaction(
          bricksByPoint -- brick.points,
          remaining ++ brick.above(bricksByPoint),
          disintegrated + brick,
          queued ++ brick.above(bricksByPoint)
        )
      case Some((brick, remaining)) =>
        chainReaction(
          bricksByPoint,
          remaining,
          disintegrated,
          queued
        )
      case None =>
        disintegrated.size - 1

  def settle(bricks: Set[Brick]): Set[Brick] =
    bricks.toList.sortBy(_.minZ).foldLeft(Map.empty[Pos, Int] -> Set.empty[Brick]){case ((skyline, result), brick) =>
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
