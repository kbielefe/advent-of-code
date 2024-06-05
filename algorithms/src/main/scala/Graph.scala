package algorithms

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Edge[V, E](from: V, to: V, props: E) derives CanEqual:
  override def toString: String =
    s"$from-[$props]->$to"

class Graph[V, E] private (val incomingEdges: Map[V, Set[Edge[V, E]]], val outgoingEdges: Map[V, Set[Edge[V, E]]], val noIncoming: Set[V], val vertices: Set[V])(using CanEqual[V, V], CanEqual[E, E]) derives CanEqual:
  override def equals(other: Any): Boolean =
    other.asInstanceOf[Matchable] match
      case o: Graph[V, E] @unchecked => incomingEdges == o.incomingEdges && outgoingEdges == o.outgoingEdges
      case _ => false

  def toposort: Iterator[V] =
    def helper(graph: Graph[V, E]): Iterator[V] =
      graph.noIncoming.headOption match
        case None =>
          Iterator.empty
        case Some(v) =>
          Iterator(v) ++ helper(graph - v)
    helper(this)

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

  def edges: Set[Edge[V, E]] =
    incomingEdges.values.toSet.flatten ++ outgoingEdges.values.toSet.flatten

  def -(v: V): Graph[V, E] =
    val newIncoming = outgoingEdges(v).foldLeft(incomingEdges - v)((incomingEdges, edge) => incomingEdges.delMulti(edge.to, edge))
    val newOutgoing = incomingEdges(v).foldLeft(outgoingEdges - v)((outgoingEdges, edge) => outgoingEdges.delMulti(edge.from, edge))
    val newNoIncoming = noIncoming - v ++ outgoingEdges(v).filter(edge => incomingEdges(edge.to).forall(_.from == v)).map(_.to)
    new Graph(newIncoming, newOutgoing, newNoIncoming, vertices - v)

  def +(v: V): Graph[V, E] =
    new Graph(incomingEdges, outgoingEdges, noIncoming + v, vertices + v)

object Graph:
  def fromEdges[V, E](edges: IterableOnce[Edge[V, E]])(using CanEqual[V, V], CanEqual[E, E]): Graph[V, E] =
    val edgeSet = edges.iterator.to(Set)
    val vertices = edgeSet.flatMap(e => Set(e.from, e.to))
    val incomingEdges = edgeSet.groupBy(_.to).view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
    val outgoingEdges = edgeSet.groupBy(_.from).view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
    val noIncoming = vertices.filter(incomingEdges(_).isEmpty)
    new Graph[V, E](incomingEdges, outgoingEdges, noIncoming, vertices)

  def empty[V, E](using CanEqual[V, V], CanEqual[E, E]): Graph[V, E] =
    new Graph[V, E](
      Map.empty.withDefaultValue(Set.empty),
      Map.empty.withDefaultValue(Set.empty),
      Set.empty,
      Set.empty
    )
