package day18

import parse.given
import algorithms.{*, given}
import algorithms.Grid.{Neighbors, Pos}

object Puzzle extends runner.Day[Grid, Int, Int]:

  given Neighbors = Grid.NSEWNeighbors
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
    findAllKeys(Set('@'), pairs, allKeys, Set.empty)

  def part2(input: Grid): Int =
    val center = input.find('@').get
    val updatedMap = input
      .updated(center.north, '#')
      .updated(center.east,  '#')
      .updated(center.south, '#')
      .updated(center.west,  '#')
      .updated(center.north.west, '↖')
      .updated(center.north.east, '↗')
      .updated(center.south.west, '↙')
      .updated(center.south.east, '↘')
    val robots = Set('↖', '↗', '↙', '↘')
    val allKeys = input.charSet.filter(_.isLower) ++ robots
    val pairs = allKeys.map(updatedMap.find(_).get).toList.combinations(2).flatMap{
      case List(x, y) =>
        val path = updatedMap.aStar(y).getPath(x)
        val distance = path.size - 1 // path includes start
        val doorsAlongPath = path.map(updatedMap(_)).filter(_.isUpper).toSet
        List((updatedMap(x), updatedMap(y)) -> (distance, doorsAlongPath), (updatedMap(y), updatedMap(x)) -> (distance, doorsAlongPath))
      case _ => throw new Exception("Invalid combination of 2")
    }.toList.groupMap(_._1._1){case ((x, y) -> (distance, doors)) => (y, distance, doors)}.map((x, y) => (x, y.filter(_._2 > 0)))
    findAllKeys(robots, pairs, allKeys, robots)

  given Cache[(Set[Char], Set[Char]), Int] = Cache.empty

  def findAllKeys(
    current: Set[Char],
    pairs: Map[Char, List[(Char, Int, Set[Char])]],
    allKeys: Set[Char],
    foundKeys: Set[Char]): Memoized[(Set[Char], Set[Char]), Int] =
    val found = foundKeys ++ current
    if found == allKeys then
      0
    else
      val unlockedDoors = found.map(_.toUpper)
      val unblockedPaths = current.flatMap(from => pairs(from).map((to, distance, doors) => (from, to, distance, doors)))
        .filterNot(path => found.contains(path._2))
        .filter(path => (path._4 -- unlockedDoors).isEmpty)
      unblockedPaths.map{case (from, to, distance, doors) =>
        val newCurrent = current - from + to
        distance + Memoize((newCurrent, found), findAllKeys(newCurrent, pairs, allKeys, found))
      }.min
