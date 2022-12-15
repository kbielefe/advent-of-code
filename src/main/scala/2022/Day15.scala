package advent2022
import datastructures.*

object Day15:
  def part1(input: List[String]): Int = answer(input)
  def part2(input: List[String]): Int = answer(input)

  private def answer(input: List[String]): Int =
    val sensors = input.map{
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" => Sensor(sx.toInt, sy.toInt, Beacon(bx.toInt, by.toInt))
    }
    val row = sensors.flatMap(_.interval(2000000)).reduce(_ | _)
    if row.contains(3897332) then row.size - 1 else row.size

  case class Beacon(x: Int, y: Int)
  case class Sensor(x: Int, y: Int, closestBeacon: Beacon):
    def radius: Int = Math.abs(x - closestBeacon.x) + Math.abs(y - closestBeacon.y)
    def interval(rowY: Int): Option[Intervals[Int]] =
      Option.when(radius >= Math.abs(y - rowY))(x +- (radius - Math.abs(y - rowY)))
