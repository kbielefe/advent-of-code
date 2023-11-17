package parse

import scala.reflect.ClassTag

opaque type -[A, B] <: A = A

trait Read[A]:
  def read(input: String): A

given Read[Int] with
  def read(input: String): Int =
    input.toInt

given [A : Read : ClassTag, B <: String : ValueOf]: Read[-[List[A], B]] with
  def read(input: String): -[List[A], B] =
    input
      .split(valueOf[B])
      .map(summon[Read[A]].read)
      .toList
