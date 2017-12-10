import Math.max
import scala.collection.GenSeq

final class Circular[A] (v: Vector[A]) {
  def slice(from: Int, until: Int): Vector[A] = {
    val length = until - from
    val beforeWrap = v.slice(wrapI(from), v.size)
    val iterations = if (length <= beforeWrap.size) 0 else (length - beforeWrap.size) / v.size + 1
    val afterWrap = (1 to iterations).foldLeft(beforeWrap){case (result, _) => result ++ v}
    afterWrap take length
  }

  def patch(from: Int, that: GenSeq[A], replaced: Int): Circular[A] = {
    val (before, after) = v splitAt wrapI(from)
    val replacedAfter = after drop wrapI(replaced)
    val replacedBefore = before drop max(0, wrapI(replaced) - after.size) 
    val (newAfter, newBefore) = that.toVector splitAt after.size
    new Circular(newBefore ++ replacedBefore ++ newAfter ++ replacedAfter)
  }

  override def toString: String = v.mkString("Circular(", ", ", ")")

  private def wrapI(i: Int): Int = i % v.size
}
