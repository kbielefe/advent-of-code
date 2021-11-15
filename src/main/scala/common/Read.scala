trait Read[A]:
  def read(input: String): A

given [A : Read]: Read[List[A]] with
  def read(input: String): List[A] =
    input.split("\n").toList.map(summon[Read[A]].read)

given Read[EmptyTuple] with
  def read(input: String): EmptyTuple =
    EmptyTuple

given [H : Read, T <: Tuple : Read]: Read[H *: T] with
  def read(input: String): H *: T =
    summon[Read[H]].read(input) *: summon[Read[T]].read(input)

given Read[Int] with
  def read(input: String): Int =
    input
      .dropWhile(!_.isDigit)
      .takeWhile(_.isDigit)
      .toInt

given Read[String] with
  def read(input: String): String =
    input
