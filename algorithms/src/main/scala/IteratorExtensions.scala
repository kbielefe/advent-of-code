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

  def group(using CanEqual[A, A]): Iterator[Iterator[A]] =
    def helper(i: Iterator[A]): Iterator[Iterator[A]] =
      if i.hasNext then
        val next = i.next
        val (prefix, suffix) = i.span(_ == next)
        Iterator(Iterator(next) ++ prefix) ++ helper(suffix)
      else
        Iterator.empty
    helper(i)
