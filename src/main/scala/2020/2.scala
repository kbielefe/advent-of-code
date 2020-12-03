package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day2 extends StringsDay[Long, Long](2020, 2) {
  case class Password(min: Int, max: Int, char: Char, password: String) {
    def valid1: Boolean = {
      val count = password.count(_ == char)
      count >= min && count <= max
    }

    def valid2: Boolean = {
      val first = password(min - 1)
      val second = password(max - 1)
      (first == char && second != char) || (first != char && second == char)
    }
  }

  object Password {
    def apply(input: String): Password = {
      val regex = """^(\d+)-(\d+) (.): (.*)$""".r
      input match {
        case regex(min, max, char, password) => Password(min.toInt, max.toInt, char.head, password)
      }
    }
  }

  override def part1(input: Observable[String], visQueue: VisQueue): Task[Long] =
    ListVis[Password](visQueue, password => s"$password ${password.valid1}")(input.map(Password(_))).filter(_.valid1).countL

  override def part2(input: Observable[String], visQueue: VisQueue): Task[Long] =
    ListVis[Password](visQueue, password => s"$password ${password.valid2}")(input.map(Password(_))).filter(_.valid2).countL
}
