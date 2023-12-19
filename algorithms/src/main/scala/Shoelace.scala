package algorithms
import math.Integral.Implicits.infixIntegralOps

/** Find the area of a polygon given its vertices in order. First vertex is also the last.
 *  With integer values, need to use pick's theorem as this only finds internal points.
 */
def shoelace[N](points: Vector[(N, N)])(using n: Integral[N]): N =
  n.abs((0 until points.size).map{i =>
    val next = if i == (points.size - 1) then 0 else i + 1
    (points(i)._1 * points(next)._2) - (points(next)._1 * points(i)._2)
  }.sum / n.fromInt(2))

/** Pick's theorem: Find total area of a polygon, including the perimeter,
 *  given number of interior points and points on the boundary */
def picks[N](interior: N, boundary: N)(using n: Integral[N]): N =
  interior + boundary / n.fromInt(2) + n.one
