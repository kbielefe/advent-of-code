package datastructures
import scala.collection.GenSeq

final class Circular[+A] (protected val v: Vector[A])(using CanEqual[A, A]) derives CanEqual {
  def slice(from: Long, until: Long): Vector[A] = {
    val length = until - from
    val beforeWrap = v.slice(wrapI(from), v.size)
    val iterations = if (length <= beforeWrap.size) 0 else (length - beforeWrap.size) / v.size + 1
    val afterWrap = (1 to iterations.toInt).foldLeft(beforeWrap){case (result, _) => result ++ v}
    afterWrap take length.toInt
  }

  def patch[B >: A](from: Long, that: GenSeq[B], replaced: Long)(using CanEqual[B, B]): Circular[B] = {
    val (before, after) = v splitAt wrapI(from)
    val replacedAfter = after drop wrapI(replaced)
    val replacedBefore = before drop Math.max(0, wrapI(replaced) - after.size)
    val (newAfter, newBefore) = that.toVector splitAt after.size
    new Circular(newBefore ++ replacedBefore ++ newAfter ++ replacedAfter)
  }

  def size: Long = v.size

  def map[B](f: A => B)(using CanEqual[B, B]): Circular[B] =
    new Circular(v.map(f))

  def indexOf[B >: A](elem: B): Long =
    v.indexOf(elem)

  def indexWhere(p: A => Boolean): Long =
    v.indexWhere(p)

  def delete(index: Long): Circular[A] =
    patch(index, Vector.empty, 1)

  def insert[B >: A](index: Long, elem: B)(using CanEqual[B, B]): Circular[B] =
    patch(index, Vector(elem), 0)

  def apply(i: Long): A = v(wrapI(i))

  def grouped(groupSize: Int): Iterator[Circular[A]] = {
    v grouped(groupSize) map {new Circular(_)}
  }

  def reduceLeft[B >: A](op: (B, A) => B): B = v.reduceLeft(op)

  def move(from: Long, to: Long): Circular[A] =
    delete(from).insert(to, apply(from))

  override def toString: String = v.mkString("Circular(", ", ", ")")

  override def equals(other: Any): Boolean = other match {
    case that: Circular[A] => (0 until v.size).iterator.map(n => v.drop(n) ++ v.take(n)).exists(_ == that.v)
    case _                 => false
  }

  private def wrapI(i: Long): Int =
    if v.isEmpty then
      0
    else if i < 0 then
      (((i % v.size) + v.size) % v.size).toInt
    else
      (i % v.size).toInt
}
