package algorithms

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Edge[V, E](from: V, to: V, props: E)

trait Graph[V, E]:
  def vertices: Set[V]
  def edges: Set[Edge[V, E]]

  def toposort: Iterator[V] =
    ???
