package advent2022

object Day21:
  def part1(input: List[String]): Long =
    val monkeyMap = monkeys(input)
    evaluate(substitute(monkeyMap, monkeyMap("root")))

  def part2(input: List[String]): Long =
    val monkeyMap = monkeys(input).updated("humn", Human())
    val pnhm = substitute(monkeyMap, monkeyMap("pnhm"))
    val zvcm = substitute(monkeyMap, monkeyMap("zvcm"))
    solve(pnhm, zvcm)

  sealed trait Expression
  case class Add(x: Expression, y: Expression)      extends Expression
  case class Subtract(x: Expression, y: Expression) extends Expression
  case class Multiply(x: Expression, y: Expression) extends Expression
  case class Divide(x: Expression, y: Expression)   extends Expression
  case class Const(x: Int)                          extends Expression
  case class Reference(monkey: String)              extends Expression
  case class Human()                                extends Expression

  private def monkeys(input: List[String]): Map[String, Expression] =
    input.map{
      case s"$monkey: $x + $y" => monkey -> Add(Reference(x), Reference(y))
      case s"$monkey: $x - $y" => monkey -> Subtract(Reference(x), Reference(y))
      case s"$monkey: $x * $y" => monkey -> Multiply(Reference(x), Reference(y))
      case s"$monkey: $x / $y" => monkey -> Divide(Reference(x), Reference(y))
      case s"$monkey: $x"      => monkey -> Const(x.toInt)
    }.toMap

  private def substitute(monkeyMap: Map[String, Expression], monkey: Expression): Expression = monkey match
    case Add(x, y)         => Add(substitute(monkeyMap, x), substitute(monkeyMap, y))
    case Subtract(x, y)    => Subtract(substitute(monkeyMap, x), substitute(monkeyMap, y))
    case Multiply(x, y)    => Multiply(substitute(monkeyMap, x), substitute(monkeyMap, y))
    case Divide(x, y)      => Divide(substitute(monkeyMap, x), substitute(monkeyMap, y))
    case Const(x)          => monkey
    case Human()           => monkey
    case Reference(monkey) => substitute(monkeyMap, monkeyMap(monkey))

  private def evaluate(monkey: Expression): Long = monkey match
    case Add(x, y)         => evaluate(x) + evaluate(y)
    case Subtract(x, y)    => evaluate(x) - evaluate(y)
    case Multiply(x, y)    => evaluate(x) * evaluate(y)
    case Divide(x, y)      => evaluate(x) / evaluate(y)
    case Const(x)          => x.toLong
    case Reference(monkey) => ???
    case Human()           => ???

  private def solve(lhs: Expression, rhs: Expression): Long =
    lhs match
      case Add(x, y)      if containsHuman(x) => solve(x, Subtract(rhs, y))
      case Add(x, y)      if containsHuman(y) => solve(y, Subtract(rhs, x))
      case Subtract(x, y) if containsHuman(x) => solve(x, Add(rhs, y))
      case Subtract(x, y) if containsHuman(y) => solve(y, Subtract(x, rhs))
      case Divide(x, y)   if containsHuman(x) => solve(x, Multiply(rhs, y))
      case Divide(x, y)   if containsHuman(y) => solve(y, Divide(x, rhs))
      case Multiply(x, y) if containsHuman(x) => solve(x, Divide(rhs, y))
      case Multiply(x, y) if containsHuman(y) => solve(y, Divide(rhs, x))
      case _ => evaluate(rhs)

  private def containsHuman(expression: Expression): Boolean = expression match
    case Human()        => true
    case Const(_)       => false
    case Add(x, y)      => containsHuman(x) || containsHuman(y)
    case Divide(x, y)   => containsHuman(x) || containsHuman(y)
    case Subtract(x, y) => containsHuman(x) || containsHuman(y)
    case Multiply(x, y) => containsHuman(x) || containsHuman(y)
    case Reference(_)   => ???
