package common
import Math.max

final class Circular[A] (protected val v: Vector[A]) {
  def slice(from: Int, until: Int): Vector[A] = {
    val length = until - from
    val beforeWrap = v.slice(wrapI(from), v.size)
    val iterations = if (length <= beforeWrap.size) 0 else (length - beforeWrap.size) / v.size + 1
    val afterWrap = (1 to iterations).foldLeft(beforeWrap){case (result, _) => result ++ v}
    afterWrap take length
  }

  def patch(from: Int, that: Seq[A], replaced: Int): Circular[A] = {
    val (before, after) = v splitAt wrapI(from)
    val replacedAfter = after drop wrapI(replaced)
    val replacedBefore = before drop max(0, wrapI(replaced) - after.size)
    val (newAfter, newBefore) = that.toVector splitAt after.size
    new Circular(newBefore ++ replacedBefore ++ newAfter ++ replacedAfter)
  }

  def apply(i: Int): A = v(wrapI(i))

  def grouped(groupSize: Int): Iterator[Circular[A]] = {
    v grouped(groupSize) map {new Circular(_)}
  }

  def reduceLeft[B >: A](op: (B, A) => B): B = v.reduceLeft(op)

  override def toString: String = v.mkString("Circular(", ", ", ")")

  override def equals(other: Any): Boolean = other match {
    case that: Circular[A] => that.v == v
    case _                 => false
  }

  // TODO: Normalize negative numbers
  private def wrapI(i: Int): Int = if (v.isEmpty) 0 else i % v.size
}
