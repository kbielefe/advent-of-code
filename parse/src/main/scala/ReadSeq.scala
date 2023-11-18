package parse

import scala.reflect.ClassTag

opaque type -[A, B] <: A = A
opaque type ~[A, B] <: A = A

trait ReadSeq[C[_]]:
  def readSeq[A : Read : ClassTag](input: Array[String]): C[A]

given ReadSeq[List] with
  def readSeq[A : Read : ClassTag](input: Array[String]): List[A] =
    input.map(summon[Read[A]].read).toList

given delimitedSeq[C[_]: ReadSeq, A : Read : ClassTag, B <: String : ValueOf]: Read[-[C[A], B]] with
  def read(input: String): -[C[A], B] =
    summon[ReadSeq[C]].readSeq[A](input.split(valueOf[B]))
