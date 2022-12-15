package datastructures
import math.Numeric.Implicits.given
import math.Ordering.Implicits.given
import scala.annotation.tailrec

class Intervals[A](private[datastructures] val pairs: List[(A, A)])(using n: Numeric[A]):
  def |(other: Intervals[A]): Intervals[A] =
    @tailrec
    def merge(unmerged: List[(A, A)], merged: List[(A, A)]): List[(A, A)] = unmerged match
      case Nil => merged.reverse
      case head :: Nil => (head :: merged).reverse
      case left :: right :: tail =>
        if left._2 >= right._1 - n.one then
          val newEnd = if left._2 >= right._2 then left._2 else right._2
          merge((left._1, newEnd) :: tail, merged)
        else
          merge(right :: tail, left :: merged)

    new Intervals(merge((pairs ++ other.pairs).sorted, List.empty))

  def contains(element: A): Boolean =
    pairs.exists((start, end) => element >= start && element <= end)

  def size: A = pairs.map((start, end) => end - start + n.one).sum

  override def toString: String =
    pairs.map((start, end) => s"$start -> $end").mkString(", ")

object Intervals:
  def apply[A : Numeric](start: A, end: A): Intervals[A] =
    new Intervals(List(start -> end))

  def apply[A : Numeric](pair: (A, A)): Intervals[A] =
    new Intervals(List(pair))

extension [A : Numeric] (number: A)
  def +-(other: A): Intervals[A] =
    Intervals(number - other, number + other)
