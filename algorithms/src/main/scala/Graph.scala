package algorithms

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Edge[V, E](from: V, to: V, props: E)

class Graph[V, E] private (val vertices: Set[V], val edges: Set[Edge[V, E]])(using CanEqual[V, V], CanEqual[E, E]):
  def incomingEdges(v: V): Set[Edge[V, E]] =
    edges.filter(_.to == v)

  def outgoingEdges(v: V): Set[Edge[V, E]] =
    edges.filter(_.from == v)

  def noIncoming: Set[V] =
    vertices.filter(incomingEdges(_).isEmpty)

  def toposort: Iterator[V] =
    def helper(noIncoming: Queue[V], graph: Graph[V, E]): Iterator[V] =
      noIncoming.dequeueOption match
        case None =>
          Iterator.empty
        case Some((v, remaining)) =>
          val newGraph = graph - v
          Iterator(v) ++ helper(remaining ++ newGraph.noIncoming, newGraph)
    helper(Queue.from(noIncoming), this)

  def reachableFrom(v: V): Graph[V, E] =
    @tailrec
    def helper(toVisit: Queue[V], visited: Set[V], accum: Set[Edge[V, E]]): Set[Edge[V, E]] =
      toVisit.dequeueOption match
        case Some((v, remaining)) =>
          val newEdges = outgoingEdges(v)
          val newVertices = newEdges.map(_.to) -- visited - v
          helper(remaining ++ newVertices, visited + v, accum ++ newEdges)
        case None => accum
    val edges = helper(Queue(v), Set(v), Set.empty)
    Graph.fromEdges(edges) + v

  def -(v: V): Graph[V, E] =
    new Graph(vertices - v, edges.filter(edge => edge.from == v || edge.to == v))

  def +(v: V): Graph[V, E] =
    new Graph(vertices + v, edges)

object Graph:
  def fromEdges[V, E](edges: IterableOnce[Edge[V, E]])(using CanEqual[V, V], CanEqual[E, E]): Graph[V, E] =
    val edgeSet = edges.iterator.to(Set)
    new Graph[V, E](edgeSet.flatMap(e => Set(e.from, e.to)), edgeSet)
