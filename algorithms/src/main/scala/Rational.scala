package algorithms

import math.Numeric.Implicits.infixNumericOps

class Rational[N: Integral](val num: N, val denom: N)

given [N](using n: Integral[N])(using CanEqual[N, N]): Fractional[Rational[N]] with
  def minus(x: Rational[N], y: Rational[N]): Rational[N] =
    new Rational(x.num * y.denom - x.denom * y.num, x.denom * y.denom)

  def plus(x: Rational[N], y: Rational[N]): Rational[N] =
    new Rational(x.num * y.denom + x.denom * y.num, x.denom * y.denom)

  def times(x: Rational[N], y: Rational[N]): Rational[N] =
    new Rational(x.num * y.num, x.denom * y.denom)

  def div(x: Rational[N], y: Rational[N]): Rational[N] =
    new Rational(x.num * y.denom, x.denom * y.num)

  def negate(x: Rational[N]): Rational[N] =
    new Rational(-x.num, x.denom)

  def toDouble(x: Rational[N]): Double = n.toDouble(x.num) / n.toDouble(x.denom)
  def toFloat(x: Rational[N]): Float = n.toFloat(x.num) / n.toFloat(x.denom)
  def toInt(x: Rational[N]): Int = if x.denom == n.one then n.toInt(x.num) else x.toDouble.toInt
  def toLong(x: Rational[N]): Long = if x.denom == n.one then n.toLong(x.num) else x.toDouble.toLong
  def compare(x: Rational[N], y: Rational[N]): Int = (x - y).toInt
  def fromInt(x: Int): Rational[N] = new Rational(n.fromInt(x), n.one)
  def parseString(str: String): Option[Rational[N]] = n.parseString(str).map(new Rational(_, n.one))
