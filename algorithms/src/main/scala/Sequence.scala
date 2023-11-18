package algorithms

import math.Integral.Implicits.*

object Sequence:
  def fibonacci[N](using n: Integral[N]): LazyList[N] =
    n.zero #:: n.one #:: fibonacci[N].zip(fibonacci[N].tail).map((x, y) => x + y)

  def primes[N: Integral]: LazyList[N] = ???
