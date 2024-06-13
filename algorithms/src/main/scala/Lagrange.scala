package algorithms

import scala.math.Integral.Implicits.infixIntegralOps

// Least common denominator is stored separately to allow dividing all at once
// at the end to avoid loss of precision.
class Lagrange[N] private (val coefficients: List[N], val denom: N)(using N: Integral[N]):
  def apply(x: N): N =
    val powersOfX = Iterator.iterate(N.one)(_ * x)
    val num = powersOfX.zip(coefficients).map(_ * _).sum
    assert(num % denom == 0, "Not evenly divisible")
    num / denom

/** Given a list of n (x, y) points, gives a polynomial of degree n-1 that maps
 *  to those points.
 */
object Lagrange:
  def apply[N: Integral](points: IndexedSeq[(N, N)]): Lagrange[N] =
    def x(i: Int): N = points(i)._1
    def y(i: Int): N = points(i)._2

    val lcm = Mod.lcm((0 until points.size).iterator.map{term =>
      (0 until points.size).iterator.filter(_ != term).map(i => x(term) - x(i)).product
    })
    val coeffs = (0 until points.size).iterator.map{term =>
      val denom = (0 until points.size).iterator.filter(_ != term).map(i => x(term) - x(i)).product
      coefficients((0 until points.size).iterator.filter(_ != term).map(x).toList).map(_ * lcm * y(term) / denom)
    }.reduceLeft((x, y) => x.zip(y).map(_ + _))
    new Lagrange(coeffs, lcm)

  // Returned in increasing order of degree, i.e. with the constant first
  // Example of zeros for (x - 3)(x - 5) would be List(3, 5)
  def coefficients[N](zeros: List[N])(using N: Integral[N]): List[N] = zeros match
    case Nil => List.empty
    case zero :: Nil => List(-zero, N.one)
    case zero :: remaining =>
      val expanded = for
        (x, xIndex) <- List(-zero, N.one).zipWithIndex
        (y, yIndex) <- coefficients(remaining).zipWithIndex
      yield (x * y, xIndex + yIndex)
      expanded.groupMapReduce(_._2)(_._1)(_ + _).toList.sortBy(_._1).map(_._2)
