package parse

import scala.reflect.ClassTag

trait ReadSeq[C[_]]:
  def readSeq[A : Read : ClassTag](input: Array[String]): C[A]

given ReadSeq[List] with
  def readSeq[A : Read : ClassTag](input: Array[String]): List[A] =
    input.map(summon[Read[A]].read).toList

given ReadSeq[Vector] with
  def readSeq[A : Read : ClassTag](input: Array[String]): Vector[A] =
    input.map(summon[Read[A]].read).toVector

given ReadSeq[Set] with
  def readSeq[A : Read : ClassTag](input: Array[String]): Set[A] =
    input.map(summon[Read[A]].read).toSet

given ReadSeq[Iterator] with
  def readSeq[A : Read : ClassTag](input: Array[String]): Iterator[A] =
    input.map(summon[Read[A]].read).iterator
