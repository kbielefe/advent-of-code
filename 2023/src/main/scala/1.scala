package day1
import parse.{*, given}

type I = List[String]
given Read[I] = Read("\n")

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.map(calibrationValue(false)).sum

  def part2(input: I): Int =
    input.map(calibrationValue(true)).sum

  def calibrationValue(replaceWords: Boolean)(line: String): Int =
    val d = digits(line, replaceWords)
    d.head * 10 + d.last

  val numberWords = List(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  )

  val digitsByWord = numberWords.zipWithIndex.map((word, index) => (word, index + 1)).toMap

  def digits(line: String, replaceWords: Boolean): List[Int] =
    line.tails.filterNot(_.isEmpty).flatMap{part =>
      if part.head.isDigit then
        Some(part.head.asDigit)
      else if replaceWords then
        numberWords.flatMap(word => if part.startsWith(word) then digitsByWord.get(word) else None).headOption
      else
        None
    }.toList
