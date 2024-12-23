package day23
import algorithms.*
import parse.{*, given}

type Connections = Map[String, Set[String]]

given Read[(String, String)] = Read[(String, String)]("-")
given Read[List[(String, String)]] = Read("\n")
given Read[Connections] = summon[Read[List[(String, String)]]].map: pairs =>
  pairs.flatMap((l, r) => List((l, r), (r, l))).foldLeft(Map.empty[String, Set[String]]){case (map, (k, v)) => map.addMulti(k, v)}

object Puzzle extends runner.Day[Connections, Int, String]:
  def part1(connections: Connections): Int =
    val startsWithT = connections.keySet.filter(_.startsWith("t"))
    startsWithT
      .flatMap(key => connections.get(key).map(_.map(x => List(key, x))))
      .flatten
      .flatMap{case List(l, r) => ((connections(l) - r) & (connections(r) - l)).map(x => Set(l, r, x))}
      .size

  def part2(connections: Connections): String =
    connections.keySet
      .map(maxConnected(connections))
      .maxBy(_.size)
      .toList
      .sorted
      .mkString(",")

  def maxConnected(connections: Connections)(key: String): List[String] =
    (1 to (connections(key).size + 1))
      .flatMap(size => (connections(key) + key).toList.combinations(size).filter(areAllConnected(connections)))
      .maxBy(_.size)

  def areAllConnected(connections: Connections)(keys: List[String]): Boolean =
    keys.map(key => connections(key) + key).reduceLeft(_ & _).size == keys.size
