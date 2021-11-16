package puzzleparse

trait Read[A]:
  def read(input: String): A
  def readPartial(input: String): (A, String) =
    (read(input), "")

given [A : Read]: Read[List[A]] with
  def read(input: String): List[A] =
    input.split("\n").toList.map(summon[Read[A]].read)

given Read[EmptyTuple] with
  def read(input: String): EmptyTuple =
    EmptyTuple

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  def read(input: String): H *: T =
    val (h, remainder) = summon[Read[H]].readPartial(input)
    h *: summon[Read[T]].read(remainder)

given Read[Int] with
  def read(input: String): Int =
    input.toInt

  override def readPartial(input: String): (Int, String) =
    val result = input
      .dropWhile(!_.isDigit)
      .takeWhile(_.isDigit)
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
