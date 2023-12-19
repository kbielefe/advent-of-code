package day18
import parse.{*, given}
import algorithms.{picks, shoelace}

case class Dig(dir: Char, dist: Int, color: String):
  def fromColor: Dig =
    val newDist = Integer.parseInt(color.drop(1).take(5), 16)
    val newDir = color.last match
      case '0' => 'R'
      case '1' => 'D'
      case '2' => 'L'
      case '3' => 'U'
    Dig(newDir, newDist, color)

given Read[Dig] = Read("""(U|D|L|R) (\d+) \((.+)\)""".r)

type I = List[Dig] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    answer(input)

  def part2(input: I): Long =
    answer(input.map(_.fromColor))

  def answer(input: List[Dig]): Long =
    val vertices = input.scanLeft((0L, 0L)){case ((row, col), dig) =>
      dig.dir match
        case 'U' => (row - dig.dist, col)
        case 'D' => (row + dig.dist, col)
        case 'L' => (row, col - dig.dist)
        case 'R' => (row, col + dig.dist)
    }.toVector.drop(1)
    val border = input.map(_.dist).sum
    picks(shoelace(vertices), border)
