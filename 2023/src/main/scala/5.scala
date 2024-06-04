package day5
import parse.{*, given}
import algorithms.{Intervals, takeUntil}

case class Range(destStart: Long, sourceStart: Long, length: Long):
  def sourceInterval: Intervals[Long] =
    Intervals(sourceStart, sourceStart + length - 1)

  def intervals(min: Long, max: Long): Intervals[Long] =
    Intervals(destStart + min - sourceStart, destStart + max - sourceStart)
      .trimBelow(destStart)
      .trimAbove(destStart + length - 1)

case class Almanac(source: String, dest: String, ranges: List[Range]):
  val sourceIntervals = ranges.map(_.sourceInterval).reduceLeft(_ | _)

  def intervals(in: Intervals[Long]): Intervals[Long] =
    val defaults = in - sourceIntervals
    ranges.map(range => in.flatMap(range.intervals)).reduceLeft(_ | _) | defaults

case class Input(seeds: List[Long], maps: List[Almanac]):
  def lowestLocation: Long =
    seeds.map(seed => List(seed, 1)).map(locationFromRange).min

  def lowestLocationFromRange: Long =
    seeds.grouped(2).map(locationFromRange).min

  def locationFromRange(range: List[Long]): Long =
    val List(min, length) = range: @unchecked
    val initialIntervals = Intervals(min, min + length - 1)
    almanacPath.foldLeft(initialIntervals)((intervals, almanac) => almanac.intervals(intervals)).min

  val almanacsBySource =
    maps.map(almanac => (almanac.source -> almanac)).toMap

  def almanacPath: Iterator[Almanac] =
    Iterator
      .iterate(almanacsBySource("seed"))(almanac => almanacsBySource(almanac.dest))
      .takeUntil(_.dest == "location")

type I = Input
given Read[I] = Read("""(?s)seeds: ([^\n]+)\n\n(.+)""".r)
given Read[Almanac] = Read("""(?s)(.+)-to-(.+) map:\n(.+)""".r)
given Read[Range] = Read(" ")
given la: Read[List[Almanac]] = Read("\n\n")
given ll: Read[List[Long]] = Read(" ")
given lr: Read[List[Range]] = Read("\n")

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(almanac: I): Long =
    almanac.lowestLocation

  def part2(almanac: I): Long =
    almanac.lowestLocationFromRange
