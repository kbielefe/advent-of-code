package algorithms

import parse.*
import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait Graph[A]:
  def neighbors(node: A): Iterator[A]

object Graph:
  def fromId[A, B](list: IterableOnce[A], id: A => B, neighborIds: A => IterableOnce[B]): Graph[A] = new Graph[A] {
    val nodeMap: Map[B, A] = list.iterator.map(node => id(node) -> node).toMap
    def neighbors(node: A): Iterator[A] =
      neighborIds(node).iterator.map(nodeMap.apply)
  }

  extension [A](a: A)(using g: Graph[A])
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
              node.neighbors.filterNot(visited.contains).foldLeft(accum)((accum, neighbor) => accum.addMulti(neighbor, node))
            )
          case None =>
            accum
      helper(Queue(a), Set.empty, Map.empty)

    /** Produce a topological sorting of the graph. */
    def toposort(using CanEqual[A, A]): Iterator[A] =
      toposort(Queue(a), getIncoming)

    private def toposort(noIncoming: Queue[A], incoming: Map[A, Set[A]])(using CanEqual[A, A]): Iterator[A] =
      noIncoming.dequeueOption match
        case None =>
          Iterator.empty
        case Some((node, remaining)) =>
          val outgoing = node.neighbors.toSet
          val newNoIncoming = outgoing.filter(next => incoming(next) == Set(node))
          val newIncoming = outgoing.foldLeft(incoming)((incoming, next) => incoming.delMulti(next, node))
          Iterator(node) ++ toposort(remaining ++ newNoIncoming, newIncoming)

    def longestPaths(using CanEqual[A, A]): Map[A, Int] =
      val incoming = getIncoming
      toposort(Queue(a), incoming).foldLeft(Map.empty[A, Int]){(maxLengths, node) =>
        maxLengths + (node -> (incoming.getOrElse(node, Set.empty).map(x => maxLengths.getOrElse(x, 0)).max + 1))
      }

    /** Recursively visit all neighbors of a node, producing a summary value B at
     *  the leaves, and reducing that value at non-leaf nodes. If a node is
     *  visited multiple times, use its cached summary value on subsequent
     *  visits. Returns None if there are no leaves.
     */
    def mapReduce[B](map: A => Option[B])(reduce: (B, B) => B)(using CanEqual[B, B]): Option[B] =
      def helper(cache: Map[A, Option[B]])(node: A, depth: Int): (Map[A, Option[B]], Option[B]) =
        map(node) match
          case None =>
            val (newCache, optB) = g.neighbors(node).foldLeft[(Map[A, Option[B]], Option[B])]((cache, None)){case ((cache, prev), neighbor) =>
              val (newCache, optB) = cache.get(neighbor) match
                case None          => helper(cache)(neighbor, depth + 1)
                case Some(None)    => (cache, None)
                case Some(Some(b)) => (cache, Some(b))
              val newPrev = (prev, optB) match
                case (None,    None)    => None
                case (Some(p), None)    => Some(p)
                case (None,    Some(b)) => Some(b)
                case (Some(p), Some(b)) => Some(reduce(p, b))
              (newCache, newPrev)
            }
            (newCache + (node -> optB), optB)
          case Some(b) =>
            (cache + (node -> Some(b)), Some(b))
      end helper
      helper(Map.empty)(a, 0)._2

  given [A](using tree: Tree[A]): Graph[A] with
    def neighbors(node: A): Iterator[A] = tree.children(node)

  given [A : Graph, B]: Graph[A ~ B] with
    def neighbors(node: A ~ B): Iterator[A ~ B] =
      summon[Graph[A]].neighbors(node).asInstanceOf[Iterator[A ~ B]]

  given [A : Graph, B]: Graph[A - B] with
    def neighbors(node: A - B): Iterator[A - B] =
      summon[Graph[A]].neighbors(node).asInstanceOf[Iterator[A - B]]
