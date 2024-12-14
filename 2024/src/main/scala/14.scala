package day14
import algorithms.Grid, Grid.Pos
import parse.{*, given}

val width = 101
val height = 103

case class Robot(px: Int, py: Int, vx: Int, vy: Int):
  def move: Robot =
    Robot(((px + vx) % width + width) % width, ((py + vy) % height + height) % height, vx, vy)

  def quadrant: Option[(Int, Int)] =
    Option.unless(px == width / 2 || py == height / 2)(2 * px / width -> 2 * py / height)

given Read[Robot] = Read("""p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r)
given Read[List[Robot]] = Read("\n")

object Puzzle extends runner.Day[List[Robot], Int, Int]:
  def part1(robots: List[Robot]): Int =
    safetyFactor(Iterator.iterate(robots)(move).drop(100).next)

  def part2(robots: List[Robot]): Int =
    Iterator.iterate(robots)(move).zipWithIndex.find(containsTop.tupled).map(_._2).get

  def move(robots: List[Robot]): List[Robot] =
    robots.map(_.move)

  def safetyFactor(robots: List[Robot]): Int =
    robots.flatMap(_.quadrant).groupMapReduce(identity)(_ => 1)(_ + _).values.product

  def containsTop(robots: List[Robot], index: Int): Boolean =
    val positions = robots.map(robot => Pos(robot.py, robot.px)).toSet
    positions.exists: robot =>
      positions.contains(robot.south.east) &&
      positions.contains(robot.south.west) &&
      positions.contains(robot.south.east.south.east) &&
      positions.contains(robot.south.west.south.west) &&
      positions.contains(robot.south.east.south.east.south.east) &&
      positions.contains(robot.south.west.south.west.south.west)
