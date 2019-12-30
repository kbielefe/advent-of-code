package common.graph
import monix.eval.Task
import monix.reactive.Observable

/** 
 * This package contains a graph implementation with an API that is better
 * suited than scala-graph to the types of problems encountered in Advent of
 * Code.
 *
 * It consists of a set of nodes, which each have a position field and a data
 * field, and uses functions to dynamically generate edges.
 */
class Graph[P, N](nodes: Set[Node[P, N]], neighbors: P => Set[P]) {

  def filter(p: N => Boolean): Set[Node[P, N]] = nodes.filter(node => p(node.data))

  def reachable(from: N, edgeTo: N => Boolean, target: N => Boolean): Observable[N] = {
    nodeFromData(from).map{start =>
      depthFirst(start, edgeTo).map(_.data).filter(target)
    }.getOrElse(Observable.empty)
  }

  def nodeFromData(data: N): Option[Node[P, N]] =
    nodes.find(_.data == data)

  def nodeFromPosition(position: P): Option[Node[P, N]] =
    nodes.find(_.position == position)

  def neighborNodes(from: Node[P, N]): Set[Node[P, N]] = {
    neighbors(from.position).flatMap(nodeFromPosition)
  }

  def depthFirst(from: Node[P, N], edgeTo: N => Boolean): Observable[Node[P, N]] = {
    def helper(from: Node[P, N], visited: Set[Node[P, N]]): Observable[(Set[Node[P, N]], Node[P, N])] = {
      if (nodes.contains(from) && !visited.contains(from)) {
        val unvisitedNeighbors = neighborNodes(from) -- visited
        val reachableNeighbors = unvisitedNeighbors.filter(neighbor => edgeTo(neighbor.data))
        Observable.fromIterable(reachableNeighbors).flatScan0(visited -> from){case ((visited, prev), neighbor) => helper(neighbor, visited + prev)}
      } else {
        Observable.empty
      }
    }
    helper(from, Set.empty).map(_._2)
  }
}

object Graph {
  def fromLines(lines: Observable[String]): Task[Graph[Square, Char]] = {
    val nodes = lines.zipWithIndex.flatMap{case (line, y) =>
      Observable.fromIterable(line)
        .zipWithIndex
        .map{case (char, x) => Node(Square(x.toInt, y.toInt), char)}
    }

    def neighbors(from: Square): Set[Square] = {
      val x = from.x
      val y = from.y
      Set(Square(x + 1, y), Square(x - 1, y), Square(x, y + 1), Square(x, y - 1))
    }

    nodes.toListL.map{nodes => new Graph(nodes.toSet, neighbors)}
  }
}

case class Square(x: Int, y: Int)

case class Node[P, N](position: P, data: N)
