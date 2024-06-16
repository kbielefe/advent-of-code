package algorithms

import spire.compat.integral
import spire.math.Rational

class Lagrange(val coefficients: List[Rational]):
  def apply(x: Rational): Rational =
    val powersOfX = Iterator.iterate(Rational.one)(_ * x)
    powersOfX.zip(coefficients).map(_ * _).sum

/** Given a list of n (x, y) points, gives a polynomial of degree n-1 that maps
 *  to those points.
 */
object Lagrange:
  def apply(points: IndexedSeq[(Rational, Rational)]): Lagrange =
    def x(i: Int): Rational = points(i)._1
    def y(i: Int): Rational = points(i)._2

    val coeffs = (0 until points.size).iterator.map{term =>
      val denom = (0 until points.size).iterator.filter(_ != term).map(i => x(term) - x(i)).product
      coefficients((0 until points.size).iterator.filter(_ != term).map(x).toList).map(_ * y(term) / denom)
    }.reduceLeft((x, y) => x.zip(y).map(_ + _))
    new Lagrange(coeffs)

  // Returned in increasing order of degree, i.e. with the constant first
  // Example of zeros for (x - 3)(x - 5) would be List(3, 5)
  def coefficients(zeros: List[Rational]): List[Rational] = zeros match
    case Nil => List.empty
    case zero :: Nil => List(-zero, Rational.one)
    case zero :: remaining =>
      val expanded = for
        (x, xIndex) <- List(-zero, Rational.one).zipWithIndex
        (y, yIndex) <- coefficients(remaining).zipWithIndex
      yield (x * y, xIndex + yIndex)
      expanded.groupMapReduce(_._2)(_._1)(_ + _).toList.sortBy(_._1).map(_._2)
