package puzzleparse
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import scala.reflect.ClassTag
import io.circe.Json
import io.circe.parser.parse

trait Read[A]:
  def read(input: String): A
  def readPartial(input: String): (A, String) =
    (read(input), "")

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
    input.split(delim).map(_.trim).filterNot(_.isEmpty).toList.map(summon[Read[A]].read)

given [A : Read]: Read[Vector[A]] with
  def read(input: String): Vector[A] =
    summon[Read[List[A]]].read(input).toVector

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
    input.trim.toInt

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

given Read[Char] with
  def read(input: String): Char =
    input.head

given Read[Digit] with
  def read(input: String): Digit =
    input.head.asDigit.asInstanceOf[Digit]

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

given [A : Read]: Read[Grid[A]] with
  def read(input: String): Grid[A] =
    val delim = if input.linesIterator.filterNot(_.isEmpty).next.contains(" ") then "\\s+" else ""
    input.linesIterator.filterNot(_.isEmpty).zipWithIndex.flatMap{(line, row) =>
      line.split(delim).filterNot(_.isEmpty).zipWithIndex.map{(elem, col) =>
        (Pos(row, col), summon[Read[A]].read(elem))
      }
    }.toMap.asInstanceOf[Grid[A]]

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

given [A : Read, B : Read]: Read[Header[A, B]] with
  def read(input: String): Header[A, B] =
    val header = summon[Read[A]].read(input.linesIterator.next)
    val body = summon[Read[B]].read(input.linesIterator.drop(1).mkString("\n"))
    Header(header, body)

given Read[Json] with
  def read(input: String): Json =
    parse(input) match
      case Right(json) => json
      case Left(error) => throw new Exception(s"couldn't parse json $input $error")

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
