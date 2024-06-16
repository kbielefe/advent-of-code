package algorithms.breeze

import breeze.linalg.DenseVector
import spire.compat.numeric
import spire.math.Numeric
import spire.syntax.all.*
import scala.reflect.ClassTag

extension [V: Numeric: ClassTag](v: DenseVector[V])
  infix def cross(other: DenseVector[V]): DenseVector[V] =
    assert(v.length == 3 && other.length == 3, "Can only take cross product of a 3D vector")
    DenseVector(v(1)*other(2) - v(2)*other(1), v(2)*other(0) - v(0)*other(2), v(0)*other(1) - v(1)*other(0))

  def magnitude: V =
    v.valuesIterator.map(x => x * x).sum.sqrt
