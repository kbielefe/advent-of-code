package day16
import parse.{*, given}

case class Valve(name: String, rate: Int, tunnels: List[String] - ", ") derives ReadProduct
type I = List[Valve ~ """Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnels? leads? to valves? (.+)"""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.foreach(println)
    ???

  def part2(input: I): Int =
    ???
