package advent2022
import scala.annotation.tailrec

object Day25:
  def part1(input: List[String]): String =
    input.reduce(sum)

  def part2(input: List[String]): String =
    ???

  private def sum(lhs: String, rhs: String): String =
    @tailrec
    def helper(lhs: String, rhs: String, accum: String, carry: Int): String =
      if lhs.isEmpty && rhs.isEmpty && carry == 0 then
        accum
      else
        val lhsHead = if lhs.isEmpty then 0 else digitToInt(lhs.head)
        val rhsHead = if rhs.isEmpty then 0 else digitToInt(rhs.head)
        val lhsTail = if lhs.isEmpty then "" else lhs.tail
        val rhsTail = if rhs.isEmpty then "" else rhs.tail
        val result = carry + lhsHead + rhsHead
        val newCarry = if result < -2 then -1 else if result > 2 then 1 else 0
        val newInt = if newCarry == 1 then result - 5 else if newCarry == -1 then 5 + result else result
        helper(lhsTail, rhsTail, s"${intToDigit(newInt)}$accum", newCarry)
    helper(lhs.reverse, rhs.reverse, "", 0)

  private def digitToInt(digit: Char): Int = digit match
    case '-' => -1
    case '=' => -2
    case n   => n.asDigit

  private def intToDigit(int: Int): Char = int match
    case -1 => '-'
    case -2 => '='
    case n  => s"$n".head
