package day7
import parse.{*, given}

case class Equation(test: Long, operands: List[Long]):
  def canBeTrue(operators: List[(Long, Long) => Long]): Boolean =
    def dfs(accum: Long, operands: List[Long]): Boolean =
      if operands.isEmpty then
        accum == test
      else
        operators.exists(op => dfs(op(accum, operands.head), operands.tail))
    dfs(operands.head, operands.tail)

given operandList: Read[List[Long]] = Read(" ")
given Read[Equation] = Read(": ")
given Read[List[Equation]] = Read("\n")

object Puzzle extends runner.Day[List[Equation], Long, Long]:
  def part1(equations: List[Equation]): Long =
    equations
      .filter(_.canBeTrue(List(_ * _, _ + _)))
      .map(_.test)
      .sum

  def part2(equations: List[Equation]): Long =
    equations
      .filter(_.canBeTrue(List(_ * _, _ + _, concat)))
      .map(_.test)
      .sum

  def concat(lhs: Long, rhs: Long): Long =
    (lhs.toString + rhs.toString).toLong
