package algorithms
import scala.annotation.tailrec
import cats.*

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

given Traverse[Iterator] with
  def foldLeft[A, B](fa: Iterator[A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: Iterator[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)

  def traverse[G[_], A, B](fa: Iterator[A])(f: A => G[B])(using Applicative[G]): G[Iterator[B]] =
    fa.foldLeft(Applicative[G].pure(Iterator.empty[B]))((accum, a: A) =>
        Applicative[G].map2(accum, f(a))(_ ++ Iterator(_))
    )
