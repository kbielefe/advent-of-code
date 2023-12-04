package algorithms
import scala.annotation.tailrec

extension [A](i: Iterator[A])
  // Like takeWhile, but inclusive of the value that caused it to stop.
  def takeUntil(p: A => Boolean): Iterator[A] =
    val (prefix, suffix) = i.span(x => !p(x))
    prefix ++ suffix.take(1)

  def lastOption: Option[A] =
    @tailrec
    def helper(last: Option[A]): Option[A] =
      if i.hasNext then
        helper(Some(i.next))
      else
        last
    helper(None)
