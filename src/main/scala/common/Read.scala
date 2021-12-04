package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.ClassTag

trait Read[A]:
  def read(input: String): A
  def readPartial(input: String): (A, String) =
    (read(input), "")

given [A : Read]: Read[List[A]] with
  def read(input: String): List[A] =
    input.split("[\r\n]+").toList.map(summon[Read[A]].read)

given Read[EmptyTuple] with
  def read(input: String): EmptyTuple =
    EmptyTuple

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  def read(input: String): H *: T =
    val (h, remainder) = summon[Read[H]].readPartial(input)
    h *: summon[Read[T]].read(remainder)

given Read[Nat] with
  def read(input: String): Nat =
    input.toInt.asInstanceOf[Nat]

  override def readPartial(input: String): (Nat, String) =
    if !input.exists(_.isDigit) then
      throw Exception(s"'$input' did not contain any numbers")
    val result = input
      .dropWhile(!_.isDigit)
      .takeWhile(_.isDigit)
      .toInt
      .asInstanceOf[Nat]
    val remainder = input
      .dropWhile(!_.isDigit)
      .dropWhile(_.isDigit)
    (result, remainder)

given Read[Long] with
  def read(input: String): Long =
    input.toLong

given Read[Binary] with
  def read(input: String): Binary =
    Integer.parseInt(input, 2).asInstanceOf[Binary]

given Read[Int] with
  def read(input: String): Int =
    input.toInt

  override def readPartial(input: String): (Int, String) =
    if !input.exists(_.isDigit) then
      throw Exception(s"'$input' did not contain any numbers")
    val result = input
      .dropWhile(char => !char.isDigit && char != '+' && char != '-')
      .takeWhile(char => char.isDigit || char == '+' || char == '-')
      .toInt
    val remainder = input
      .dropWhile(!_.isDigit)
      .dropWhile(_.isDigit)
    (result, remainder)

given Read[String] with
  def read(input: String): String =
    input

given Read[Letters] with
  def read(input: String): Letters =
    input.asInstanceOf[Letters] // TODO: fail if not letters

  override def readPartial(input: String): (Letters, String) =
    val result = input
      .dropWhile(!_.isLetter)
      .takeWhile(_.isLetter)
      .asInstanceOf[Letters]
    val remainder = input
      .dropWhile(!_.isLetter)
      .dropWhile(_.isLetter)
    (result, remainder)

given Read[Letter] with
  def read(input: String): Letter =
    input.head.asInstanceOf[Letter] // TODO: fail if more than one character or if not a letter

  override def readPartial(input: String): (Letter, String) =
    val result = input
      .dropWhile(!_.isLetter)
      .head
      .asInstanceOf[Letter]
    val remainder = input
      .dropWhile(!_.isLetter)
      .drop(1)
    (result, remainder)

given Read[Grid] with
  def read(input: String): Grid =
    input.linesIterator.zipWithIndex.flatMap{(line, row) =>
      line.zipWithIndex.map{(char, col) =>
        ((row, col), char)
      }
    }.toMap.asInstanceOf[Grid]

given [A : Read : ClassTag]: Read[MultiLine[A]] with
  def read(input: String): MultiLine[A] =
    input.split("\n\n").map(summon[Read[A]].read).toList.asInstanceOf[MultiLine[A]]

given [K : Read, V : Read]: Read[Map[K, V]] with
  def read(input: String): Map[K, V] =
    input
      .split("\\s")
      .map(_.split(":") match {case Array(k, v) => (summon[Read[K]].read(k.trim), summon[Read[V]].read(v.trim))})
      .toMap

given [D <: String & Singleton: ValueOf, K : Read, V : Read]: Read[DMap[D, K, V]] with
  def read(input: String): DMap[D, K, V] =
    input
      .split("\n")
      .map(_.split(valueOf[D]) match {case Array(k, v) => (summon[Read[K]].read(k.trim), summon[Read[V]].read(v.trim))})
      .toMap
      .asInstanceOf[DMap[D, K, V]]

given [D <: String & Singleton: ValueOf, A : Read : ClassTag]: Read[DList[D, A]] with
  def read(input: String): DList[D, A] =
    input
      .split(valueOf[D])
      .map(summon[Read[A]].read)
      .toList
      .asInstanceOf[DList[D, A]]

object Read:
  inline given derived[T](using m: Mirror.ProductOf[T]): Read[T] =
    new Read[T]:
      def read(input: String): T =
        m.fromProduct(fold[m.MirroredElemTypes](input))

  private inline def fold[T <: Tuple](input: String): Tuple =
    inline erasedValue[T] match
      case _: EmptyTuple => EmptyTuple
      case _: (t *: ts)  =>
        val (result, remainder) = summonInline[Read[t]].readPartial(input)
        result *: fold[ts](remainder)
