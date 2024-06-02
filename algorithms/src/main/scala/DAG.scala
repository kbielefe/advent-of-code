package algorithms

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/** Directed Acyclic Graph */
trait DAG[A] extends Graph[A]

object DAG:
  extension [A](a: A)(using g: DAG[A])
    def neighbors: Iterator[A] = g.neighbors(a)

    /** Produce a map from all nodes to predecessors to that node. */
    def getIncoming: Map[A, Set[A]] =
      @tailrec
      def helper(toVisit: Queue[A], visited: Set[A], accum: Map[A, Set[A]]): Map[A, Set[A]] =
        toVisit.dequeueOption match
          case Some((node, remaining)) if visited.contains(node) =>
            helper(remaining, visited, accum)
          case Some((node, remaining)) =>
            helper(
              remaining ++ node.neighbors,
              visited + node,
              node.neighbors.foldLeft(accum)((accum, neighbor) => accum.addMulti(neighbor, node))
            )
          case None =>
            accum
      helper(Queue(a), Set.empty, Map.empty)

    /** Produce a topological sorting of the graph. */
    def toposort(using CanEqual[A, A]): Iterator[A] =
      def helper(noIncoming: Queue[A], incoming: Map[A, Set[A]]): Iterator[A] =
        noIncoming.dequeueOption match
          case None =>
            Iterator.empty
          case Some((node, remaining)) =>
            val outgoing = node.neighbors.toSet
            val newNoIncoming = outgoing.filter(next => incoming(next) == Set(node))
            val newIncoming = outgoing.foldLeft(incoming)((incoming, next) => incoming.delMulti(next, node))
            Iterator(node) ++ helper(remaining ++ newNoIncoming, newIncoming)
      helper(Queue(a), getIncoming)
