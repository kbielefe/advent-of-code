package day16

import parse.given

sealed trait Expr
case class Sum(xs: Seq[Expr]) extends Expr
case class Mult(x: Expr, y: Expr) extends Expr
case class Negate(x: Expr) extends Expr
case class InputDigit(x: Int, index: Int) extends Expr
case class PatternDigit(x: Int) extends Expr

object Puzzle extends runner.Day[String, String, String]:
  def part1(input: String): String =
    val inputDigits = input.map(_.asDigit).zipWithIndex.map((digit, index) => InputDigit(digit, index + 1))
    println(toString(simplify((1 to 5).foldLeft(inputDigits.take(10))(applyPattern).drop(1).head)))
    ???

  def part2(input: String): String =
    val messageOffset = input.take(7).toInt
    ???

  def evaluate(expr: Expr): Int = expr match
    case Sum(xs) => xs.map(evaluate).sum
    case Mult(x, y) => evaluate(x) * evaluate(y)
    case Negate(x) => -1 * evaluate(x)
    case InputDigit(x, _) => x
    case PatternDigit(x) => x

  def toString(expr: Expr): String = expr match
    case Sum(xs) => "(" + xs.map(toString).mkString("+") + ")"
    case Mult(x, y) => s"${toString(x)} * ${toString(y)}"
    case Negate(x) => s"-${toString(x)}"
    case InputDigit(_, d) => s"d$d"
    case PatternDigit(x) => x.toString

  def simplify(expr: Expr): Expr = expr match
    case Sum(xs) =>
      val nonZero = xs.filter(x => evaluate(x) != 0).map(simplify)
      if nonZero.isEmpty then PatternDigit(0) else Sum(nonZero)
    case Mult(x, y) =>
      val ex = evaluate(x)
      val ey = evaluate(y)
      if ex == 0 || ey == 0 then
        PatternDigit(0)
      else if ex == -1 then
        Negate(simplify(y))
      else if ex == 1 then
        simplify(y)
      else if ey == 1 then
        simplify(x)
      else
        Mult(simplify(x), simplify(y))
    case other => other

  // digit starts at 1
  def pattern(digit: Int): Iterator[Expr] =
    Iterator.continually(Iterator(0, 1, 0, -1).map(PatternDigit(_)))
      .flatten
      .flatMap(x => Iterator.fill(digit)(x))
      .drop(1)

  def applyPattern(input: Seq[Expr], phase: Int): Seq[Expr] =
    (1 to input.size).toList.map{digit => Sum(pattern(digit).zip(input.iterator).map{case (x, y) => Mult(x, y)}.toSeq)}
