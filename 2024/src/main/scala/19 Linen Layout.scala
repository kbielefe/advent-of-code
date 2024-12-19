package day19
import algorithms.{Memoize, Memoized, Cache}
import parse.{*, given}

case class Towels(patterns: List[String], desiredDesigns: Set[String]):
  given Cache[String, Long] = Cache.empty

  def countPossible: Int =
    desiredDesigns.count(design => count(design) > 0)

  def designCounts: Long =
    desiredDesigns.map(count).sum

  def count(design: String): Memoized[String, Long] =
    if design.isEmpty then
      1
    else
      patterns
        .filter(design.startsWith)
        .map(pattern => Memoize(design.drop(pattern.size), count(design.drop(pattern.size))))
        .sum

given Read[List[String]] = Read(", ")
given Read[Set[String]] = Read("\n")
given Read[Towels] = Read("\n\n")

object Puzzle extends runner.Day[Towels, Int, Long]:
  def part1(towels: Towels): Int =
    towels.countPossible

  def part2(towels: Towels): Long =
    towels.designCounts
