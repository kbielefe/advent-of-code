package algorithms

import scala.math.Integral.Implicits.infixIntegralOps

object Lagrange:
  def apply[N: Integral](points: IndexedSeq[(N, N)], x: N): N =
    def X(i: Int): N = points(i)._1
    def Y(i: Int): N = points(i)._2

    (0 until points.size).iterator.map{term =>
      val num   = (0 until points.size).iterator.filter(_ != term).map(i => x - X(i)).product
      val denom = (0 until points.size).iterator.filter(_ != term).map(i => X(term) - X(i)).product
      num * Y(term) / denom
    }.sum
