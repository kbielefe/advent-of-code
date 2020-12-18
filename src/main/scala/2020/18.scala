package advent2020

import common._

object Day18 extends SyncStringsDay[BigInt, BigInt](2020, 18) {
  override def part1(input: Seq[String]): BigInt =
    input.map(shuntingYard(noPrecedence)).map(evaluateRpn).sum

  override def part2(input: Seq[String]): BigInt =
    input.map(shuntingYard(reversePrecedence)).map(evaluateRpn).sum

  private val noPrecedence = Map('+' -> 0, '*' -> 0)

  private val reversePrecedence = Map('+' -> 1, '*' -> 0)

  private def shuntingYard(precedence: Map[Char, Int])(input: String): String = {
    val (operatorStack, output) = input.filter(_ != ' ').foldLeft((List.empty[Char], List.empty[Char])){case ((operatorStack, output), token) =>
      if (token.isDigit)
        (operatorStack, token :: output)
      else if (token == '(')
        (token :: operatorStack, output)
      else if (token == ')') {
        val newOutput = operatorStack.takeWhile(_ != '(')
        val (first::newOperatorStack) = operatorStack.dropWhile(_ != '(')
        if (first == '(')
          (newOperatorStack, newOutput.reverse ++ output)
        else
          (newOperatorStack, first :: (newOutput.reverse ++ output))
      } else {
        val newOutput = operatorStack.takeWhile(op => op != '(' && precedence(op) >= precedence(token))
        val newOperatorStack = operatorStack.dropWhile(op => op != '(' && precedence(op) >= precedence(token))
        (token :: newOperatorStack, newOutput.reverse ++ output)
      }
    }
    (operatorStack.reverse ++ output).reverse.mkString
  }

  private def evaluateRpn(input: String): BigInt =
    input.foldLeft(List.empty[BigInt]){case (stack, token) =>
      if (token.isDigit) {
        BigInt(token.asDigit) :: stack
      } else if (token == '*') {
        val lhs :: rhs :: tail = stack
        (lhs * rhs) :: tail
      } else {
        val lhs :: rhs :: tail = stack
        (lhs + rhs) :: tail
      }
    }.head
}
