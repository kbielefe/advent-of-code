trait Read[A]:
  def read(input: String): A

given [A : Read]: Read[List[A]] with
  def read(input: String): List[A] =
    input.split("\n").toList.map(summon[Read[A]].read)

given Read[Int] with
  def read(input: String): Int =
    input.toInt
