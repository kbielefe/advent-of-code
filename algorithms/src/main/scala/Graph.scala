package algorithms

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Edge[V, E](from: V, to: V, props: E) derives CanEqual:
  override def toString: String =
    s"$from-[$props]->$to"

class Graph[V, E] private (val vertices: Set[V], val edges: Set[Edge[V, E]])(using CanEqual[V, V], CanEqual[E, E]) derives CanEqual:
  override def equals(other: Any): Boolean =
    other.asInstanceOf[Matchable] match
      case o: Graph[V, E] @unchecked => vertices == o.vertices && edges == o.edges
      case _ => false

  override def toString: String =
    s"Vertices:\n${vertices.mkString("\n")}\nEdges:\n${edges.mkString("\n")}"

  def incomingEdges(v: V): Set[Edge[V, E]] =
    edges.filter(_.to == v)

  def outgoingEdges(v: V): Set[Edge[V, E]] =
    edges.filter(_.from == v)

  def noIncoming: Set[V] =
    vertices.filter(incomingEdges(_).isEmpty)

  def toposort: Iterator[V] =
    def helper(noIncoming: Queue[V], queued: Set[V], graph: Graph[V, E]): Iterator[V] =
      noIncoming.dequeueOption match
        case None =>
          Iterator.empty
        case Some((v, remaining)) =>
          val newGraph = graph - v
          val newQueued = newGraph.noIncoming -- queued
          Iterator(v) ++ helper(remaining ++ newQueued, queued ++ newQueued, newGraph)
    helper(Queue.from(noIncoming), noIncoming, this)

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
    new Graph(vertices - v, edges.filterNot(edge => edge.from == v || edge.to == v))

  def +(v: V): Graph[V, E] =
    new Graph(vertices + v, edges)

object Graph:
  def fromEdges[V, E](edges: IterableOnce[Edge[V, E]])(using CanEqual[V, V], CanEqual[E, E]): Graph[V, E] =
    val edgeSet = edges.iterator.to(Set)
    new Graph[V, E](edgeSet.flatMap(e => Set(e.from, e.to)), edgeSet)

  def empty[V, E](using CanEqual[V, V], CanEqual[E, E]): Graph[V, E] =
    new Graph[V, E](Set.empty, Set.empty)
