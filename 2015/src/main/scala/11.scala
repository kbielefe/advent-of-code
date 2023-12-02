package day11
import parse.{*, given}

object Puzzle extends runner.Day[String, String, String]:
  def part1(input: String): String =
    nextValidPassword(input)

  def part2(input: String): String =
    nextValidPassword(nextValidPassword(input))

  def nextValidPassword(password: String): String =
    Iterator.iterate(password)(increment)
      .drop(1)
      .dropWhile(!valid(_))
      .next

  def increment(password: String): String =
    if password.last == 'z' then
      increment(password.init) + 'a'
    else
      password.init + increment(password.last)

  def increment(char: Char): Char =
    (char.toInt + 1).toChar

  def valid(password: String): Boolean =
    includesStraight(password) &&
    !"iol".exists(password.contains) &&
    pairCount(password) >= 2

  def includesStraight(password: String): Boolean =
    password
      .map(_.toInt)
      .sliding(3)
      .exists{case Seq(a, b, c) => (b-a) == 1 && (c-b) == 1}

  def pairCount(password: String): Int =
    if password.size < 2 then
      0
    else if password(0) == password(1) then
      1 + pairCount(password.drop(2))
    else
      pairCount(password.drop(1))
