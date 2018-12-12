package common

object Dynamic {
  def cumulativeSums[A](input: List[List[A]])(implicit n: Numeric[A]): List[List[A]] = {
    def sumRow(above: List[A], row: List[A]): List[A] = {
      val zipped: List[(A, A, A)] = (n.zero :: above, above, row).zipped.toList
      zipped.scanLeft(n.zero){case (prev, (aboveLeft, above, curr)) =>
        n.minus(n.plus(n.plus(above, prev), curr), aboveLeft)
      } drop 1
    }

    val width: Int = input.headOption map {_.size} getOrElse 0
    val rowOfZeroes: List[A] = List.fill(width)(n.zero)
    input.scanLeft(rowOfZeroes){sumRow(_, _)} drop 1
  }
}
