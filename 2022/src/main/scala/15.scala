package advent2022
import datastructures.*

object Day15:
  def part1(input: List[String]): Int =
    val sensors = parseSensors(input)
    val beacons = sensors.map(_.closestBeacon).filter(_.y == 2000000).map(_.x).toSet
    val row = sensors.flatMap(_.interval(2000000)).reduce(_ | _)
    val containedBeacons = beacons.count(row.contains)
    row.size - containedBeacons

  def part2(input: List[String]): Long =
    val sensors = parseSensors(input)
    val fullRow = Intervals(0, 4000000)
    (0 to 4000000).iterator
      .map{y =>
        val row = sensors.flatMap(_.interval(y)).reduce(_ | _)
        y -> (fullRow - row)
      }
      .find(_._2.size == 1)
      .map((y, hole) => y.toLong + hole.elements.head.toLong * 4000000L)
      .get

  case class Beacon(x: Int, y: Int)
  case class Sensor(x: Int, y: Int, closestBeacon: Beacon):
    def radius: Int = Math.abs(x - closestBeacon.x) + Math.abs(y - closestBeacon.y)
    def interval(rowY: Int): Option[Intervals[Int]] =
      Option.when(radius >= Math.abs(y - rowY))(x +- (radius - Math.abs(y - rowY)))

  private def parseSensors(input: List[String]): List[Sensor] =
    input.map{
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        Sensor(sx.toInt, sy.toInt, Beacon(bx.toInt, by.toInt))
    }
