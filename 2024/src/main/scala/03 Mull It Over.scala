package day3
import parse.{*, given}

object Puzzle extends runner.Day[String, Int, Int]:
  val mul = """mul\((\d{1,3}),(\d{1,3})\)""".r

  def part1(input: String): Int =
    val products = mul.findAllMatchIn(input).map: m =>
      m.group(1).toInt * m.group(2).toInt
    products.sum

  def part2(input: String): Int =
    val (enabled, sum) = """mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)""".r.findAllIn(input).foldLeft((true, 0)):
      case ((true, sum), mul(left, right)) => (true, sum + left.toInt * right.toInt)
      case ((_, sum), "do()")              => (true, sum)
      case ((_, sum), "don't()")           => (false, sum)
      case ((enabled, sum), _)             => (enabled, sum)
    sum
