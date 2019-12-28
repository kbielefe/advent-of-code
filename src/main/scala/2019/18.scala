package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.LDiEdge
import scalax.collection.GraphEdge.DiEdge
import common.GraphUtils.{fromLines, Square}
import scalax.collection.edge.Implicits._

class Day18 extends DayTask[Graph[Square, LDiEdge], Int, String] {

  override def input(lines: Observable[String]) = fromLines(lines)

  private def shortestPath(from: Square, graph: Graph[Square, LDiEdge], topoSort: List[List[Square]]): Int = {
    val (thisLayer :: nextLayer) = topoSort
    if (nextLayer == Nil) {
      0
    } else {
      thisLayer.map{square =>
        graph.get(from).shortestPathTo(graph.get(square)).get.nodes.size - 1 + shortestPath(square, graph, nextLayer)
      }.min
    }
  }

  // 1900 is too low
  override def part1(graph: Graph[Square, LDiEdge]) = Task{
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
  }

  override def part2(graph: Graph[Square, LDiEdge]) = Task{"unimplemented"}
}
