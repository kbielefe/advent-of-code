package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable
import scala.util.Try

object Day4 extends MapDay[Long, Long](2020, 4) {
  private def present(passport: Map[String, String]): Boolean =
    (Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") -- passport.keySet).isEmpty

  private def valid(passport: Map[String, String]): Boolean = {
    def intRange(field: String, min: Int, max: Int): Boolean =
      Try(passport(field).toInt).filter(num => num >= min && num <= max).isSuccess

    val inRegex = """(\d+)in""".r
    val cmRegex = """(\d+)cm""".r

    val validHeight = passport("hgt") match {
      case cmRegex(num) => num.toInt >= 150 && num.toInt <= 193
      case inRegex(num) => num.toInt >= 59 && num.toInt <= 76
      case _ => false
    }

    intRange("byr", 1920, 2002) &&
    intRange("iyr", 2010, 2020) &&
    intRange("eyr", 2020, 2030) &&
    validHeight &&
    """#[0-9a-f]{6}""".r.matches(passport("hcl")) &&
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(passport("ecl")) &&
    """\d{9}""".r.matches(passport("pid"))
  }

  override def part1(input: Observable[Map[String, String]]): Task[Long] =
    input.filter(present).countL

  override def part2(input: Observable[Map[String, String]]): Task[Long] =
    input.filter(present).filter(valid).countL
}
