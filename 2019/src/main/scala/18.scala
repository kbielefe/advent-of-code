package day18

import parse.given
import algorithms.{*, given}
import algorithms.Grid.Pos
import scala.collection.mutable

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    println(input)
    val allKeys = input.charSet.filter(_.isLower)
    val pairs = (allKeys + '@').map(input.find(_).get).toList.combinations(2).flatMap{
      case List(x, y) =>
        val path = input.aStar(y).getPath(x)
        val distance = path.size - 1 // path includes start
        val doorsAlongPath = path.map(input(_)).filter(_.isUpper).toSet
        List((input(x), input(y)) -> (distance, doorsAlongPath), (input(y), input(x)) -> (distance, doorsAlongPath))
      case _ => throw new Exception("Invalid combination of 2")
    }.toList.groupMap(_._1._1){case ((x, y) -> (distance, doors)) => (y, distance, doors)}
    findAllKeys('@', pairs, allKeys + '@', Set.empty)

  def part2(input: Grid): Int =
    ???

  val cache: mutable.Map[(Char, Set[Char]), Int] = mutable.Map.empty

  def findAllKeys(current: Char, pairs: Map[Char, List[(Char, Int, Set[Char])]], allKeys: Set[Char], foundKeys: Set[Char]): Int =
    if cache.contains((current, foundKeys)) then
      cache((current, foundKeys))
    else if (foundKeys + current) == allKeys then
      0
    else
      val unlockedDoors = foundKeys.map(_.toUpper) + current.toUpper
      val unblockedPaths = pairs(current)
        .filterNot(path => foundKeys.contains(path._1))
        .filter(path => (path._3 -- unlockedDoors).isEmpty)
        .map(x => (x._1, x._2))
      if unblockedPaths.isEmpty then
        println(current)
        println(foundKeys.toList.sorted)
        pairs(current).sortBy(_._1).map((dest, distance, set) => (dest, distance, set -- unlockedDoors)).foreach(println)
      val result = unblockedPaths.map{case (next, distance) =>
        distance + findAllKeys(next, pairs, allKeys, foundKeys + current)
      }.min
      cache += ((current, foundKeys) -> result)
      result
