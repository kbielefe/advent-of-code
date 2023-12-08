package algorithms

import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

trait Modulus[N]:
  val mod: N

object Modulus:
  def apply[N](n: N): Modulus[N] = new Modulus[N]:
    val mod: N = n

opaque type Mod[N] = N

object Mod:
  def apply[N](n: N): Mod[N] = n

  def lcm[N](xs: Iterable[N])(using n: Integral[N])(using CanEqual[N, N]): N =
    xs.reduceLeft((x, y) => (x * y) / gcd(x, y))

  @tailrec
  def gcd[N](a: N, b: N)(using n: Integral[N])(using CanEqual[N, N]): N =
    if a == n.zero then b else gcd(b % a, a)

  def modInverse[N](a: N, m: N)(using n: Integral[N])(using CanEqual[N, N]): N =
    assert(m != n.one)
    @tailrec
    def helper(a: N, m: N, x: N, y: N): N =
      if a <= n.one then
        x
      else
        val q = a / m
        helper(m, a % m, y, x - q * y)

    val result = helper(a, m, n.one, n.zero)
    if result < n.zero then
      result + m
    else
      result

  /** Given a list of numbers and corresponding remainders, find a result that
   *  when divided by each number, results in the corresponding remainder.
   *
   *  All the numbers must be coprime (i.e. each pair's gcd is 1).
   *
   *  Useful for cycle detection, to find when multiple cycles "sync up."
   */
  def chineseRemainder[N](remAndNum: Iterable[(N, N)])(using n: Integral[N])(using CanEqual[N, N]): N =
    val coprime = remAndNum.map(_._2).toList.combinations(2).forall{case List(x, y) => gcd(x, y) == n.one}
    assert(coprime, "numbers must be coprime")
    val prod = remAndNum.map(_._2).product
    val result = remAndNum.map{(rem, num) =>
      val pp = prod / num
      rem * modInverse(pp, num) * pp
    }.sum
    result % prod

given [N](using CanEqual[N, N]): CanEqual[Mod[N], Mod[N]] = CanEqual.derived
given [N](using N: Integral[N]): Conversion[Int, Mod[N]] = (i: Int) => Mod(N.fromInt(i))

given [N](using n: Integral[N])(using CanEqual[N, N])(using m: Modulus[N]): Integral[Mod[N]] with
  def quot(x: Mod[N], y: Mod[N]): Mod[N] =
    n.rem(n.quot(x, y), m.mod)

  def rem (x: Mod[N], y: Mod[N]): Mod[N] =
    n.rem(n.rem(x, y), m.mod)

  def minus(x: Mod[N], y: Mod[N]): Mod[N] =
    n.rem(n.minus(x, y), m.mod)

  def plus(x: Mod[N], y: Mod[N]): Mod[N] =
    n.rem(n.plus(x, y), m.mod)

  // TODO: Prevent overflow
  def times(x: Mod[N], y: Mod[N]): Mod[N] =
    n.rem(n.times(x, y), m.mod)

  def negate(x: Mod[N]): Mod[N] =
    n.rem(n.negate(x), m.mod)

  def toDouble(x: Mod[N]): Double = n.toDouble(x)
  def toFloat(x: Mod[N]): Float = n.toFloat(x)
  def toInt(x: Mod[N]): Int = n.toInt(x)
  def toLong(x: Mod[N]): Long = n.toLong(x)
  def compare(x: Mod[N], y: Mod[N]): Int = n.compare(x, y)
  def fromInt(x: Int): Mod[N] = n.fromInt(x)
  def parseString(str: String): Option[Mod[N]] = n.parseString(str)
