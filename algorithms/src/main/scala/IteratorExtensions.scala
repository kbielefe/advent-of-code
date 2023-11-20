package algorithms

extension [A](i: Iterator[A])
  // Like takeWhile, but inclusive of the value that caused it to stop.
  def takeUntil(p: A => Boolean): Iterator[A] =
    val (prefix, suffix) = i.span(x => !p(x))
    prefix ++ suffix.take(1)
