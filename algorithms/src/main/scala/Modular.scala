package algorithms

trait Modulus[N]:
  val mod: N

object Modulus:
  def apply[N](n: N): Modulus[N] = new Modulus[N]:
    val mod: N = n

opaque type Mod[N] = N

object Mod:
  def apply[N](n: N): Mod[N] = n

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
