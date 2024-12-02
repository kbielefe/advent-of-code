package day2
import parse.{*, given}

type Report = List[Int]
type Reports = List[Report]

given Read[Report] = Read(" ")
given Read[Reports] = Read("\n")

extension (report: Report)
  def allIncreasing: Boolean =
    report.sliding(2).forall{case List(left, right) => left < right}

  def allDecreasing: Boolean =
    report.reverse.allIncreasing

  def diffWithinRange: Boolean =
    report.sliding(2).forall{case List(left, right) =>
      val diff = Math.abs(left - right)
      diff >= 1 && diff <= 3
    }

  def isSafe: Boolean =
    (allIncreasing || allDecreasing) && diffWithinRange

  def dampened: Iterator[Report] =
    def helper(prefix: Report, suffix: Report): Iterator[Report] =
      if suffix.isEmpty then
        Iterator.empty
      else
        Iterator(prefix ++ suffix.drop(1)) ++ helper(prefix.appended(suffix.head), suffix.drop(1))
    helper(List.empty, report)

object Puzzle extends runner.Day[Reports, Int, Int]:
  def part1(input: Reports): Int =
    input.count(_.isSafe)

  def part2(input: Reports): Int =
    input.count(_.dampened.exists(_.isSafe))
