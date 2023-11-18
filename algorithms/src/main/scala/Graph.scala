package algorithms

trait Graph[A]:
  def neighbors(node: A): Iterator[A]

extension [A](a: A)(using g: Graph[A])
  def neighbors: Iterator[A] = g.neighbors(a)

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
