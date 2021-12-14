package advent2021
import puzzleparse.{*, given}

object Day14:
  def part1(input: List[String]): Int =
    val template = getTemplate(input)
    val rules = getInsertionRules(input)
    val result = Iterator.iterate(template)(insertion(rules)).drop(10).next
    val sizes = result.groupBy(identity).map(_._2.size)
    sizes.max - sizes.min

  def part2(input: List[String]): Long =
    val template = getTemplate(input)
    val rules = getInsertionRules(input)
    ???

  private def getTemplate(input: List[String]): String = input.head.trim

  private def getInsertionRules(input: List[String]): Map[String, Char] =
    input.drop(1).map(_.split(" -> ")).map{case Array(x, y) => (x, y.head)}.toMap

  private def insertion(rules: Map[String, Char])(template: String): String =
    template.sliding(2).map(pair => pair.head.toString + rules(pair)).mkString + template.last
