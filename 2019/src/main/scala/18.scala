package day18

import parse.given
import algorithms.{*, given}
import algorithms.Grid.Pos
import scala.collection.mutable

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    val allKeys = input.charSet.filter(_.isLower) + '@'
    val pairs = allKeys.map(input.find(_).get).toList.combinations(2).flatMap{
      case List(x, y) =>
        val path = input.aStar(y).getPath(x)
        val distance = path.size - 1 // path includes start
        val doorsAlongPath = path.map(input(_)).filter(_.isUpper).toSet
        List((input(x), input(y)) -> (distance, doorsAlongPath), (input(y), input(x)) -> (distance, doorsAlongPath))
      case _ => throw new Exception("Invalid combination of 2")
    }.toList.groupMap(_._1._1){case ((x, y) -> (distance, doors)) => (y, distance, doors)}
    findAllKeys('@', pairs, allKeys, Set.empty)

  def part2(input: Grid): Int =
    ???

  given Cache[(Char, Set[Char]), Int] = Cache.empty

  def findAllKeys(
    current: Char,
    pairs: Map[Char, List[(Char, Int, Set[Char])]],
    allKeys: Set[Char],
    foundKeys: Set[Char]): Memoized[(Char, Set[Char]), Int] =
    val found = foundKeys + current
    if found == allKeys then
      0
    else
      val unlockedDoors = found.map(_.toUpper)
      val unblockedPaths = pairs(current)
        .filterNot(path => found.contains(path._1))
        .filter(path => (path._3 -- unlockedDoors).isEmpty)
        .map(x => (x._1, x._2))
      unblockedPaths.map{case (next, distance) =>
        distance + Memoize((next, found), findAllKeys(next, pairs, allKeys, found))
      }.min
