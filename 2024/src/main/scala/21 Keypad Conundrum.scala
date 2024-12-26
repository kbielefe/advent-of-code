package day21
import algorithms.*
import parse.{*, given}

val directional = Graph.fromEdges(List(
  Edge('^', 'A', '>'),
  Edge('^', 'v', 'v'),
  Edge('A', '^', '<'),
  Edge('A', '>', 'v'),
  Edge('<', 'v', '>'),
  Edge('v', '^', '^'),
  Edge('v', '<', '<'),
  Edge('v', '>', '>'),
  Edge('>', 'A', '^'),
  Edge('>', 'v', '<')
))

val numeric = Graph.fromEdges(List(
  Edge('A', '0', '<'),
  Edge('A', '3', '^'),
  Edge('0', 'A', '>'),
  Edge('0', '2', '^'),
  Edge('1', '4', '^'),
  Edge('1', '2', '>'),
  Edge('2', '1', '<'),
  Edge('2', '3', '>'),
  Edge('2', '5', '^'),
  Edge('2', '0', 'v'),
  Edge('3', '2', '<'),
  Edge('3', '6', '^'),
  Edge('3', 'A', 'v'),
  Edge('4', '7', '^'),
  Edge('4', '5', '>'),
  Edge('4', '1', 'v'),
  Edge('5', '4', '<'),
  Edge('5', '6', '>'),
  Edge('5', '8', '^'),
  Edge('5', '2', 'v'),
  Edge('6', '5', '<'),
  Edge('6', '9', '^'),
  Edge('6', '3', 'v'),
  Edge('7', '4', 'v'),
  Edge('7', '8', '>'),
  Edge('8', '7', '<'),
  Edge('8', '5', 'v'),
  Edge('8', '9', '>'),
  Edge('9', '8', '<'),
  Edge('9', '6', 'v')
))

given Read[List[String]] = Read("\n")
object Puzzle extends runner.Day[List[String], Long, Long]:
  given Cache[(Int, List[Char]), Long] = Cache.empty

  def part1(codes: List[String]): Long =
    codes.map(complexity(2)).sum

  def part2(codes: List[String]): Long =
    codes.map(complexity(25)).sum

  def complexity(depth: Int)(code: String): Long =
    pathCounts(code, depth) * code.take(3).toLong

  def pathCounts(code: String, depth: Int): Long =
    ("A" + code).sliding(2).map(segment => numeric.shortestPaths(segment(0), segment(1)).map(presses).map(pressCount(depth)).min).sum

  def presses(edges: List[Edge[Char, Char]]): List[Char] =
    edges.map(_.props).prepended('A').reverse

  def pressCount(depth: Int)(code: List[Char]): Memoized[(Int, List[Char]), Long] =
    //println("  " * (2 - depth) + code.mkString)
    if depth == 0 then
      code.size
    else
      ('A' :: code).sliding(2).map(segment => directional.shortestPaths(segment(0), segment(1)).map(presses).map(code => Memoize((depth - 1, code), pressCount(depth - 1)(code))).min).sum
