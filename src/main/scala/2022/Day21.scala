package advent2022
import algorithms.binarySearch
import scala.util.Try

object Day21:
  def part1(input: List[String]): BigInt =
    evaluate(monkeys(input), "root", Map.empty[String, BigInt])._1

  def part2(input: List[String]): BigInt =
    val monkeyMap = monkeys(input)
    def shout(monkey: String, n: Long): BigInt =
      val result = evaluate(monkeyMap, monkey, Map("humn" -> n))._1
      println(s"shout($monkey, $n) = $result")
      result
    val zvcm = shout("zvcm", 0)
    binarySearch[BigInt, Long](0L, Long.MaxValue, shout("pnhm", _), _ < zvcm)._2

  private def monkeys(input: List[String]): Map[String, String] =
    input.map(_.split(": ")).map{case Array(monkey, expression) => monkey -> expression}.toMap

  private def evaluate(monkeys: Map[String, String], number: String, numbers: Map[String, BigInt]): (BigInt, Map[String, BigInt]) =
    def binary(x: String, y: String, op: (BigInt, BigInt) => BigInt): (BigInt, Map[String, BigInt]) =
      import math.Integral.Implicits.infixIntegralOps
      val (xResult, xnumbers) = evaluate(monkeys, x, numbers)
      val (yResult, ynumbers) = evaluate(monkeys, y, xnumbers)
      val result = op(xResult, yResult)
      (result, ynumbers + (number -> result))

    numbers.get(number).map(_ -> numbers).getOrElse{
      Try(BigInt(number)).map(_ -> numbers).getOrElse{
        monkeys(number) match
          case s"$x * $y" => binary(x, y, _ * _)
          case s"$x / $y" =>
            println(s"$x % $y = ${evaluate(monkeys, x, numbers)._1 % evaluate(monkeys, y, numbers)._1}")
            binary(x, y, _ / _)
          case s"$x + $y" => binary(x, y, _ + _)
          case s"$x - $y" => binary(x, y, _ - _)
          case x => (BigInt(x), numbers + (number -> BigInt(x)))
      }
    }
