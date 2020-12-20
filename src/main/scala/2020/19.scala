package advent2020

import common._
import scala.util.matching.Regex

object Day19 extends MultilineSyncStringsDay[Int, Int](2020, 19) {
  override def part1(input: Seq[Seq[String]]): Int = {
    val rules = input(0).map(parseRule).toMap
    val regex = new Regex(createRegex(0, rules))
    input(1).count(regex.matches(_))
  }

  override def part2(input: Seq[Seq[String]]): Int = {
    val rule8 = (1 to 10).map(count => Iterator.fill(count)(42).mkString(" ")).mkString(" | ")
    val rule11 = (1 to 10).map(count => Iterator.fill(count)(42).mkString(" ") + " " + Iterator.fill(count)(31).mkString(" ")).mkString(" | ")
    val rules = input(0).map(parseRule).toMap + parseRule(s"8: ${rule8}") + parseRule(s"11: ${rule11}")
    val regex = new Regex(createRegex(0, rules))
    input(1).count(regex.matches(_))
  }

  private sealed trait Rule
  private case class CharRule(char: Char)        extends Rule
  private case class Refs(refs: List[List[Int]]) extends Rule

  private def parseRule(line: String): (Int, Rule) = {
    val s"$num: $rule" = line
    if (rule.contains("\"")) {
      val s""""$char"""" = rule
      (num.toInt, CharRule(char.head))
    } else {
      val refs = rule.split("\\|").map(_.trim.split(" ").filterNot(_.isEmpty).map(_.toInt).toList).toList
      (num.toInt, Refs(refs))
    }
  }

  private def createRegex(ruleNumber: Int, rules: Map[Int, Rule]): String = rules(ruleNumber) match {
    case CharRule(char) => char.toString
    case Refs(refs)     => refs.map(_.map(createRegex(_, rules)).mkString).mkString("(", "|", ")")
  }
}
