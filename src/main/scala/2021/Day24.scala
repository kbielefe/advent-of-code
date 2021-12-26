package advent2021
import scala.util.Try

object Day24:
  def part1(input: String): Long =
    val validations = input.split("inp w\n").filterNot(_.isEmpty).toList.map("inp w" :: _.split("\n").toList)
    //println(buildExpression(validations(13))('z'))
    //println(rewrite(rewrite(rewrite(buildExpression(validations(13))('z')))))
    for i <- 0 to 13 do println(rewrite(rewrite(rewrite(buildExpression(validations(i))('z')))).toString(0))
    ???

  def part2(input: List[String]): Long =
    ???

  type Register = Char
  sealed trait Expr derives CanEqual:
    def toString(prec: Int): String

  case object Inp extends Expr:
    def toString(prec: Int): String = "input"

  case class Initial(register: Register) extends Expr:
    def toString(prec: Int): String = register.toString

  case class Constant(value: Long) extends Expr:
    def toString(prec: Int): String = value.toString

  case class Add(lhs: Expr, rhs: Expr) extends Expr:
    def toString(prec: Int): String =
      if prec > 1 then
        s"(${lhs.toString(1)} + ${rhs.toString(1)})"
      else
        s"${lhs.toString(1)} + ${rhs.toString(1)}"

  case class Min(lhs: Expr, rhs: Expr) extends Expr:
    def toString(prec: Int): String =
      if prec > 1 then
        s"(${lhs.toString(1)} - ${rhs.toString(1)})"
      else
        s"${lhs.toString(1)} - ${rhs.toString(1)}"

  case class Mul(lhs: Expr, rhs: Expr) extends Expr:
    def toString(prec: Int): String =
      if prec > 2 then
        s"(${lhs.toString(2)} * ${rhs.toString(2)})"
      else
        s"${lhs.toString(2)} * ${rhs.toString(2)}"

  case class Div(lhs: Expr, rhs: Expr) extends Expr:
    def toString(prec: Int): String =
      if prec > 2 then
        s"(${lhs.toString(2)} / ${rhs.toString(2)})"
      else
        s"${lhs.toString(2)} / ${rhs.toString(2)}"

  case class Mod(lhs: Expr, rhs: Expr) extends Expr:
    def toString(prec: Int): String =
      if prec > 2 then
        s"(${lhs.toString(2)} % ${rhs.toString(2)})"
      else
        s"${lhs.toString(2)} % ${rhs.toString(2)}"

  case class Eql(lhs: Expr, rhs: Expr, eq: Expr, neq: Expr) extends Expr:
    def toString(prec: Int): String =
      s"(if ${lhs.toString(0)} == ${rhs.toString(0)} then ${eq.toString(0)} else ${neq.toString(0)})"

  val inp = "inp (.)".r
  val add = "add (.) (-?\\d+|.)".r
  val mul = "mul (.) (-?\\d+|.)".r
  val div = "div (.) (-?\\d+|.)".r
  val mod = "mod (.) (-?\\d+|.)".r
  val eql = "eql (.) (-?\\d+|.)".r

  def buildExpression(instructions: List[String]): Map[Char, Expr] =
    val registers: Map[Char, Expr] = "wxyz".map(reg => (reg -> Initial(reg))).toMap
    instructions.foldLeft(registers)((registers, instruction) => build(instruction, registers))

  def build(instruction: String, registers: Map[Char, Expr]): Map[Char, Expr] =
    def op(a: String, b: String, f: (Expr, Expr) => Expr): Map[Char, Expr] =
      val lhs = registers(a.head)
      val rhs = Try(Constant(Integer.parseInt(b))).getOrElse(registers(b.head))
      val result = f(lhs, rhs)
      registers + (a.head -> result)

    instruction match
      case inp(a)    => registers + (a.head -> Inp)
      case add(a, b) => op(a, b, Add(_, _))
      case mul(a, b) => op(a, b, Mul(_, _))
      case div(a, b) => op(a, b, Div(_, _))
      case mod(a, b) => op(a, b, Mod(_, _))
      case eql(a, b) => op(a, b, Eql(_, _, Constant(1), Constant(0)))

  def rewrite(expr: Expr): Expr = expr match
    case Mul(Constant(0), x) => Constant(0)
    case Mul(x, Constant(0)) => Constant(0)
    case Mul(Constant(1), x) => rewrite(x)
    case Mul(x, Constant(1)) => rewrite(x)
    case Mul(a, Eql(x, y, e, n)) => rewrite(Eql(rewrite(x), rewrite(y), rewrite(Mul(rewrite(a), rewrite(e))), rewrite(Mul(rewrite(a), rewrite(n)))))
    case Mul(Eql(x, y, e, n), a) => rewrite(Eql(rewrite(x), rewrite(y), rewrite(Mul(rewrite(a), rewrite(e))), rewrite(Mul(rewrite(a), rewrite(n)))))
    case Mul(Div(x, y), z) if y == z => rewrite(x)
    case Add(Constant(0), x) => rewrite(x)
    case Add(x, Constant(0)) => rewrite(x)
    case Add(Constant(x), Constant(y)) => Constant(x+y)
    case Add(x, Constant(y)) if y < 0 => rewrite(Min(rewrite(x), Constant(-y)))
    case Add(a, Eql(x, y, e, n)) => rewrite(Eql(rewrite(x), rewrite(y), rewrite(Add(rewrite(a), rewrite(e))), rewrite(Add(rewrite(a), rewrite(n)))))
    case Add(Eql(x, y, e, n), a) => rewrite(Eql(rewrite(x), rewrite(y), rewrite(Add(rewrite(a), rewrite(e))), rewrite(Add(rewrite(a), rewrite(n)))))
    case Eql(Eql(x, y, Constant(1), Constant(0)), Constant(0), e, n)  => rewrite(Eql(rewrite(x), rewrite(y), rewrite(n), rewrite(e)))
    case Eql(x, y, Eql(a, b, c, _), Eql(e, f, _, h)) if x == a && x == e && y == b && y == f => rewrite(Eql(rewrite(x), rewrite(y), rewrite(c), rewrite(h)))
    case Div(x, Constant(1)) => rewrite(x)
    case Add(x, y) => Add(rewrite(x), rewrite(y))
    case Min(x, y) => Min(rewrite(x), rewrite(y))
    case Mul(x, y) => Mul(rewrite(x), rewrite(y))
    case Div(x, y) => Div(rewrite(x), rewrite(y))
    case Mod(x, y) => Mod(rewrite(x), rewrite(y))
    case Eql(x, y, e, n) => Eql(rewrite(x), rewrite(y), rewrite(e), rewrite(n))
    case Constant(x) => Constant(x)
    case Initial(x) => Initial(x)
    case Inp => Inp
