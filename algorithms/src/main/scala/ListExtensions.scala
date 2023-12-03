package algorithms

extension [A](i: List[A])
  // Like combinations(n), but if the original list has a repeated value,
  // you'll get multiple combinations for it.
  def combinationsWithRepetitions(n: Int): Iterator[List[A]] =
    def helper(list: List[A], accum: List[A]): Iterator[List[A]] =
      if accum.size == n then
        Iterator(accum)
      else if list.isEmpty then
        Iterator.empty
      else
        helper(list.tail, list.head :: accum) ++ helper(list.tail, accum)
    helper(i, List.empty)
