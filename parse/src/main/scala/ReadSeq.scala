package parse

import scala.reflect.ClassTag
import scala.util.matching.Regex

opaque type -[A, B] <: A = A
opaque type ~[A, B] <: A = A

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

given delimitedSeq[C[_]: ReadSeq, A : Read : ClassTag, B <: String : ValueOf]: Read[C[A] - B] with
  def read(input: String): C[A] - B =
    summon[ReadSeq[C]].readSeq[A](input.split(valueOf[B]).filterNot(_.isEmpty))

given regexSeq[C[_]: ReadSeq, A : ClassTag, B <: String : ValueOf](using Read[A ~ B]): Read[C[A] ~ B] with
  def read(input: String): C[A] ~ B =
    val regex = new Regex(valueOf[B])
    val matches = regex.findAllMatchIn(input)
    val elements = matches.map(_.matched).toArray
    summon[ReadSeq[C]].readSeq[A ~ B](elements).asInstanceOf[C[A] ~ B]
