package advent2020

import common._

object Day12 extends SyncStringsDay[Int, Int](2020, 12) {
  private def parse(input: String): (Char, Int) = {
    val regex = """(.)(\d+)""".r
    val regex(char, int) = input
    (char.head, int.toInt)
  }

  override def part1(input: Seq[String]): Int = {
    val commands = input.map(parse)
    val endCoords = commands.foldLeft((0, 0, 90)){case ((x, y, dir), (char, int)) =>
      char match {
        case 'N' => (x, y + int, dir)
        case 'S' => (x, y - int, dir)
        case 'E' => (x + int, y, dir)
        case 'W' => (x - int, y, dir)
        case 'R' => (x, y, (dir + int) % 360)
        case 'L' => (x, y, (dir - int + 360) % 360)
        case 'F' => dir match {
          case 0   => (x, y + int, dir)
          case 180 => (x, y - int, dir)
          case 90  => (x + int, y, dir)
          case 270 => (x - int, y, dir)
        }
      }
    }
    Math.abs(endCoords._1) + Math.abs(endCoords._2)
  }

  override def part2(input: Seq[String]): Int = {
    val commands = input.map(parse)
    val endCoords = commands.foldLeft((0, 0, 10, 1)){case ((shipX, shipY, wayX, wayY), (char, int)) =>
      char match {
        case 'N' => (shipX, shipY, wayX, wayY + int)
        case 'S' => (shipX, shipY, wayX, wayY - int)
        case 'E' => (shipX, shipY, wayX + int, wayY)
        case 'W' => (shipX, shipY, wayX - int, wayY)
        case 'R' => int match {
          case 90  => (shipX, shipY, wayY, -1 * wayX)
          case 180 => (shipX, shipY, -1 * wayX, -1 * wayY)
          case 270 => (shipX, shipY, -1 * wayY, wayX)
        }
        case 'L' => int match {
          case 90  => (shipX, shipY, -1 * wayY, wayX)
          case 180 => (shipX, shipY, -1 * wayX, -1 * wayY)
          case 270 => (shipX, shipY, wayY, -1 * wayX)
        }
        case 'F' => (shipX + int * wayX, shipY + int * wayY, wayX, wayY)
      }
    }
    Math.abs(endCoords._1) + Math.abs(endCoords._2)
  }
}
