package common
import scala.annotation.tailrec

object Dynamic {
  /**
   * Given a list of rows from top to bottom, outputs a list of lists where
   * every element is a sum of everything above it and to its left, including itself.
   */
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

  // Returns (start of cycle, period of cycle)
  def detectCycle[A](it: Iterator[A]): Option[(Int, Int)] = {
    @tailrec
    def detectSeen(it: Iterator[A], seen: Map[A, Int], count: Int): Option[(Int, Int)] = {
      if (!it.hasNext) {
        None
      } else {
        val curr = it.next
        val startOption = seen.get(curr)
        if (startOption.isDefined)
          startOption map {start => (start, count - start)}
        else
          detectSeen(it, seen + (curr -> count), count + 1)
      }
    }

    detectSeen(it, Map.empty[A, Int], 0)
  }
}
