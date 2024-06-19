package algorithms.breeze

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.math.Semiring
import spire.compat.numeric
import spire.math.Numeric
import spire.syntax.all.*
import scala.annotation.tailrec
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*
import scala.reflect.ClassTag

extension [V: Numeric: ClassTag](v: DenseVector[V])
  infix def cross(other: DenseVector[V]): DenseVector[V] =
    assert(v.length == 3 && other.length == 3, "Can only take cross product of a 3D vector")
    DenseVector(v(1)*other(2) - v(2)*other(1), v(2)*other(0) - v(0)*other(2), v(0)*other(1) - v(1)*other(0))

  def magnitude: V =
    v.valuesIterator.map(x => x * x).sum.sqrt

extension [A: Semiring: ClassTag](m: DenseMatrix[A])
  def ^[E](e: E)(using E: Integral[E]): DenseMatrix[A] =
    assert(e >= E.zero, "power must be greater than or equal to 0")
    assert(m.rows == m.cols, "must be a square matrix")
    @tailrec
    def helper(e: E, x: DenseMatrix[A], result: DenseMatrix[A]): DenseMatrix[A] =
      if e == E.zero then
        result
      else if E.rem(e, E.fromInt(2)) == E.one then
        helper(e - E.one, x, x * result)
      else
        helper(E.quot(e, E.fromInt(2)), x * x, result)
    helper(e, m, DenseMatrix.eye[A](m.cols))
