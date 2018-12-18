package common
import scala.annotation.tailrec
import monix.tail.Iterant
import scala.language.higherKinds
import cats.effect.Sync

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

  /*
  * Returns (number of elements before cycle starts, period of cycle, repeated element)
  */
 def detectCycle[A](it: Iterator[A]): Option[(Int, Int, A)] = {
    @tailrec
    def detectSeen(it: Iterator[A], seen: Map[A, Int], count: Int): Option[(Int, Int, A)] = {
      if (!it.hasNext) {
        None
      } else {
        val curr = it.next
        val startOption = seen.get(curr)
        if (startOption.isDefined)
          startOption map {start => (start, count - start, curr)}
        else
          detectSeen(it, seen + (curr -> count), count + 1)
      }
    }

    detectSeen(it, Map.empty[A, Int], 0)
  }

 def detectCycle[F[_], A](it: Iterant[F, A])(implicit F: Sync[F]): F[Option[(Long, Long, A)]] = {
   val seen = it.zipWithIndex.scan0(Map.empty[A, Long]){case (seen, (curr, index)) =>
     seen + (curr -> index)
   }
   val cycle = it.zipWithIndex.zipMap(seen){case ((curr, index), seen) => seen.get(curr) map {start =>
     (start, index - start, curr)
   }}
   cycle.dropWhile(!_.isDefined).map{_.get}.headOptionL
 }

 def frequency[A](xs: TraversableOnce[A]): Map[A, Int] =
   xs.foldLeft(Map.empty[A, Int]){case (freq, next) => freq + (next -> (freq.getOrElse(next, 0) + 1))}
}
