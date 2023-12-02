package day2
import parse.{*, given}

case class Cubes(count: Int, color: String):
  def possible: Boolean = color match
    case "red"   => count <= 12
    case "green" => count <= 13
    case "blue"  => count <= 14

case class Round(cubes: List[Cubes ~ """(\d+) (.+)"""] - ", "):
  def possible: Boolean =
    cubes.forall(_.possible)

  def colorCount(color: String): Int =
    cubes.find(_.color == color).map(_.count).getOrElse(0)

case class Game(id: Int, rounds: List[Round ~ """(.+)"""] - "; "):
  def possible: Boolean =
    rounds.forall(_.possible)

  def max(color: String): Int =
    rounds.map(_.colorCount(color)).max

  def power: Int =
    List("red", "green", "blue").map(max).product

type I = List[Game ~ """Game (\d+): (.+)"""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.filter(_.possible).map(_.id).sum

  def part2(input: I): Int =
    input.map(_.power).sum
