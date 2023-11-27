package algorithms

import math.Integral.Implicits.*
import math.Ordering.Implicits.*
import scala.annotation.tailrec

object Modular:
  // aka extended euclidian algorithm
  // like division when solving equations mod n
  // a and n must be coprime (not have any common divisors other than 1)
  def multiplicativeInverse[N](a: N, n: N)(using integral: Integral[N]): Option[N] =
    val iterations = Iterator.iterate((integral.zero, n, integral.one, a)){case (t, r, newt, newr) =>
      val quotient = r / newr
      (newt, newr, t - quotient * newt, r - quotient * newr)
    }
    val (t, r, _, _) = iterations.dropWhile(all => !(all._4 `equiv` integral.zero)).next
    if r > integral.one then
      None
    else if t < integral.zero then
      Some(t + n)
    else
      Some(t)

  def multiply(mod: Long, a: Long, b: Long): Long =
    val high = Math.multiplyHigh(a, b)
    val low = a * b
    val highQuotient = (high / mod)
    val lowQuotient = (low / mod)
    low - (low / mod) * mod
    // high * 2^64 + low
    // (high % mod) * 2^64 + low % mod
