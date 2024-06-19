package algorithms

import _root_.breeze.math.Semiring
import cats.Functor
import cats.syntax.all.*
import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

trait Modulus[N]:
  val mod: N

object Modulus:
  def apply[N](n: N): Modulus[N] = new Modulus[N]:
    val mod: N = n

opaque type Mod[N] = N

extension [N](n: Mod[N])
  def value: N = n

object Mod:
  given [V](using s: Semiring[V], m: Modulus[V], n: Integral[V]): Semiring[Mod[V]] with
    override def zero: Mod[V] = s.zero
    override def one: Mod[V] = s.one
    override def +(a: Mod[V], b: Mod[V]): Mod[V] = normalize(a + b)
    override def *(a: Mod[V], b: Mod[V]): Mod[V] = normalize(a * b)
    override def ==(a: Mod[V], b: Mod[V]): Boolean = a == b
    override def !=(a: Mod[V], b: Mod[V]): Boolean = a != b
    def normalize(x: Mod[V]): Mod[V] =
      n.rem(n.plus(n.rem(x, m.mod), m.mod), m.mod)

  def apply[N](n: N)(using N: Integral[N])(using m: Modulus[N]): Mod[N] =
    N.rem(N.plus(N.rem(n, m.mod), m.mod), m.mod)

  def lcm[N: Integral](xs: IterableOnce[N]): N =
    xs.iterator.reduceLeft((x, y) => (x * y) / gcd(x, y))

  @tailrec
  def gcd[N](a: N, b: N)(using n: Integral[N]): N =
    if a == n.zero then b else gcd(b % a, a)

  /** a * modInverse(a) === 1 */
  def modInverse[N](a: N)(using n: Integral[N])(using m: Modulus[N]): N =
    @tailrec
    def helper(a: N, m: N, x: N, y: N): N =
      if a <= n.one then
        x
      else
        val q = a / m
        helper(m, a % m, y, x - q * y)

    val result = helper(a, m.mod, n.one, n.zero)
    if result < n.zero then
      result + m.mod
    else
      result

  /** Given a list of numbers and corresponding remainders, find a result that
   *  when divided by each number, results in the corresponding remainder.
   *
   *  All the numbers must be coprime (i.e. each pair's gcd is 1).
   *
   *  Useful for cycle detection, to find when multiple cycles "sync up."
   */
  def chineseRemainder[N](remAndNum: Iterable[(N, N)])(using n: Integral[N]): N =
    val coprime = remAndNum.map(_._2).toList.combinations(2).forall{case List(x, y) => gcd(x, y) == n.one}
    assert(coprime, "numbers must be coprime")
    val prod = remAndNum.map(_._2).product
    val result = remAndNum.map{(rem, num) =>
      given Modulus[N] = Modulus(num)
      val pp = prod / num
      rem * modInverse(pp) * pp
    }.sum
    result % prod

  given [N: Modulus](using N: Integral[N]): Conversion[Int, Mod[N]] = (i: Int) => Mod(N.fromInt(i))
  given [F[_]: Functor, A: Integral: Modulus]: Conversion[F[A], F[Mod[A]]] =
    (xs: F[A]) => xs.map(Mod(_))

  given [N](using n: Integral[N])(using m: Modulus[N]): Integral[Mod[N]] with
    def quot(x: Mod[N], y: Mod[N]): Mod[N] =
      n.rem(n.quot(x, y), m.mod)

    def rem (x: Mod[N], y: Mod[N]): Mod[N] =
      n.rem(n.rem(x, y), m.mod)

    def minus(x: Mod[N], y: Mod[N]): Mod[N] =
      normalize(n.rem(n.minus(x, y), m.mod))

    def plus(x: Mod[N], y: Mod[N]): Mod[N] =
      n.rem(n.plus(x, y), m.mod)

    def times(x: Mod[N], y: Mod[N]): Mod[N] =
      val two = n.fromInt(2)
      @tailrec
      def helper(x: N, y: N, result: N): Mod[N] =
        if y == n.zero then
          result
        else if n.rem(y, two) == n.one then
          helper(x, n.minus(y, n.one), n.rem(n.plus(result, x), m.mod))
        else
          helper(n.rem(n.times(x, two), m.mod), n.quot(y, two), result)
      helper(normalize(x), normalize(y), n.zero)

    def negate(x: Mod[N]): Mod[N] =
      normalize(n.rem(n.negate(x), m.mod))

    def normalize(x: Mod[N]): Mod[N] =
      n.rem(n.plus(x, m.mod), m.mod)

    def toDouble(x: Mod[N]): Double = n.toDouble(x)
    def toFloat(x: Mod[N]): Float = n.toFloat(x)
    def toInt(x: Mod[N]): Int = n.toInt(x)
    def toLong(x: Mod[N]): Long = n.toLong(x)
    def compare(x: Mod[N], y: Mod[N]): Int = n.compare(x, y)
    def fromInt(x: Int): Mod[N] = n.fromInt(x)
    def parseString(str: String): Option[Mod[N]] = n.parseString(str)
