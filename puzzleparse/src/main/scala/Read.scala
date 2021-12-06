package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.ClassTag

trait Read[A]:
  def read(input: String): A

given [A : Read]: Read[List[A]] with
  def read(input: String): List[A] =
    val delim =
      if input.contains("\n") then
        "[\r\n]+"
      else if input.contains(",") then
        ",\\s*"
      else if input.contains(" ") then
        "\\s+"
      else
        ""
    input.split(delim).filterNot(_.isEmpty).toList.map(summon[Read[A]].read)

given Read[EmptyTuple] with
  def read(input: String): EmptyTuple =
    EmptyTuple

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  def read(input: String): H *: T =
    ???

given [A : Read]: Read[Grid[A]] with
  def read(input: String): Grid[A] =
    val delim = if input.linesIterator.filterNot(_.isEmpty).next.contains(" ") then "\\s+" else ""
    input.linesIterator.filterNot(_.isEmpty).zipWithIndex.flatMap{(line, row) =>
      line.split(delim).filterNot(_.isEmpty).zipWithIndex.map{(elem, col) =>
        (Pos(row, col), summon[Read[A]].read(elem))
      }
    }.toMap.asInstanceOf[Grid[A]]

given [K : Read, V : Read]: Read[Map[K, V]] with
  def read(input: String): Map[K, V] =
    input
      .split("\\s")
      .map(_.split(":") match {case Array(k, v) => (summon[Read[K]].read(k.trim), summon[Read[V]].read(v.trim))})
      .toMap

object Read:
  inline given derived[T](using m: Mirror.ProductOf[T]): Read[T] =
    new Read[T]:
      def read(input: String): T =
        m.fromProduct(fold[m.MirroredElemTypes](input))

  private inline def fold[T <: Tuple](input: String): Tuple =
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts)  => ???
