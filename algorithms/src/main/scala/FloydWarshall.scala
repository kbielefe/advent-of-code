package algorithms
import math.Ordering.Implicits.*
import math.Numeric.Implicits.*

/* Find the shortest paths between all vertices in a directed weighted graph in O(V^3) time.
 * Positive or negative weights allowed.
 * No negative cycles.
 * if undirected, edges map must contain both directions.
 */
def floydWarshall[Vertex, Weight](edges: Map[(Vertex, Vertex), Weight])(using n: Numeric[Weight]): Map[(Vertex, Vertex), Weight] =
  val vertices = edges.keySet.flatMap((from, to) => Set(from, to))
  val initial = edges ++ vertices.map(v => (v, v) -> n.zero).toMap
  val iterator = for
    k <- vertices.iterator
    i <- vertices.iterator
    j <- vertices.iterator
  yield (k, i, j)
  iterator.foldLeft(initial){case (weights, (k, i, j)) =>
    val sum = for
      left  <- weights.get((i, k))
      right <- weights.get((k, j))
    yield left + right
    val shouldUpdate = (weights.get((i, j)), sum) match
      case (None, None)       => false
      case (None, Some(_))    => true
      case (Some(_), None)    => false
      case (Some(x), Some(y)) => x > y
    if shouldUpdate then weights.updated((i, j), sum.get) else weights
  }
