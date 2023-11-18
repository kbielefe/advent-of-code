package parse

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.util.matching.Regex

trait ReadProduct[T <: Product]:
  def readProduct(input: Array[String]): T

given delimitedProduct[B <: String : ValueOf, T <: Product : ReadProduct]: Read[-[T, B]] with
  def read(input: String): -[T, B] =
    summon[ReadProduct[T]]
      .readProduct(input.split(valueOf[B]))
      .asInstanceOf[-[T, B]]

given regexProduct[B <: String : ValueOf, T <: Product : ReadProduct]: Read[~[T, B]] with
  def read(input: String): ~[T, B] =
    val regex = new Regex(valueOf[B])
    regex.unapplySeq(input) match
      case Some(fields) => summon[ReadProduct[T]].readProduct(fields.toArray).asInstanceOf[~[T, B]]
      case None         => throw new Exception(s"Regex '${valueOf[B]}' did not match '$input'")

object ReadProduct:
  inline given derived[T <: Product](using m: Mirror.ProductOf[T]): ReadProduct[T] =
    new ReadProduct[T]:
      def readProduct(input: Array[String]): T =
        m.fromProduct(fold[m.MirroredElemTypes](input))

  private inline def fold[T <: Tuple](input: Array[String]): Tuple =
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts)  => summonInline[Read[t]].read(input.head) *: fold[ts](input.tail)
