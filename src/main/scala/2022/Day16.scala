package advent2022
import algorithms.floydWarshall
import scala.annotation.tailrec

object Day16:
  def part1(input: List[String]): Int =
    val valves = input.map{
      case s"Valve $name has flow rate=$flowRate; tunnel$_ lead$_ to valve$_ $tunnels" =>
        name -> Valve(name, flowRate.toInt, tunnels.split(", ").toSet)
    }.toMap
    val edges = valves.values.flatMap(valve => valve.tunnels.flatMap(tunnel => List((valve.name, tunnel) -> 1, (tunnel, valve.name) -> 1))).toMap
    val distances = floydWarshall(edges)
    val nonZeroDistances = distances.filter{case ((from, to), distance) =>
      distance != 0 && valves(to).flowRate != 0 && (from == "AA" || valves(from).flowRate != 0)
    }.view.mapValues(_ + 1).toMap
    val vertices = nonZeroDistances.toSet.flatMap{case ((from, to), distance) => Set(from, to)}
    possiblePaths(List("AA"), 30, Set("AA"), nonZeroDistances, vertices).map(pressure(nonZeroDistances, valves)).max

  def part2(input: List[String]): Int =
    ???

  case class Valve(name: String, flowRate: Int, tunnels: Set[String])

  private def pressure(distances: Map[(String, String), Int], valves: Map[String, Valve])(path: List[String]): Int =
    def times = path.sliding(2).map{case List(from, to) => distances((from, to))}.scanLeft(30)(_ - _)
    val flowRates = path.map(valves(_).flowRate)
    flowRates.zip(times).map(_ * _).sum

  private def possiblePaths(accum: List[String], timeRemaining: Int, visited: Set[String], distances: Map[(String, String), Int], vertices: Set[String]): Iterator[List[String]] =
    if timeRemaining <= 0 then
      Iterator(accum.tail.reverse)
    else
      val from = accum.head
      val tunnels = vertices -- visited
      if tunnels.isEmpty then
        Iterator(accum.reverse)
      else
        tunnels.foldLeft(Iterator.empty){(iterator, to) =>
          iterator ++ possiblePaths(to :: accum, timeRemaining - distances((from, to)), visited + to, distances, vertices)
        }
