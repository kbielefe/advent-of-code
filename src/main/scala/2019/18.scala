package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable
import common.graph._

class Day18 extends DayTask[Graph[Square, Char], String, String] {

  override def input(lines: Observable[String]) = Graph.fromLines(lines)

  // 1900 is too low
  override def part1(graph: Graph[Square, Char]) = Task{
    "1900"
  }

  /*
  def isKey(square: Square) = square.char >= 'a' && square.char <= 'z'
  def isDoor(square: Square) = square.char >= 'A' && square.char <= 'Z'
  def n(char: Char) = graph.get(graph.nodes.find(_.char == char).get)
  val start = n('@')
  val keys = graph.nodes.filter(isKey).map(node => graph.get(node))
  val doors = graph.nodes.filter(isDoor).map(node => graph.get(node))
  val edges = keys.flatMap{key => 
    val (predKeys, predDoors) = start.shortestPathTo(key).get.nodes.filter(square => isKey(square) || isDoor(square)).init.partition(isKey)
    val preds = predKeys ++ predDoors.map(door => n(door.char.toLower))
    preds map {pred => DiEdge(pred.value, key.value)}
  }
  val predGraph = Graph[Square, DiEdge]((edges ++ keys).toSeq.asInstanceOf[Seq[Param[Square, DiEdge]]]:_*)
  val topoSort = predGraph.topologicalSort.right.get.toLayered.map(_._2.toList.map(_.value)).toList
  topoSort.mkString("\n")
  shortestPath(n('@'), graph, topoSort)
  */

  override def part2(graph: Graph[Square, Char]) = Task{"unimplemented"}
}
