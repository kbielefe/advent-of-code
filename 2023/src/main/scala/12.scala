package day12
import parse.{*, given}
import algorithms.{Cache, Memoize, Memoized}

enum Spring derives CanEqual:
  case Damaged, Operational, Unknown

case class Row(springs: String, counts: List[Int]):
  import Spring.*
  def unfold: Row =
    Row(Iterator.fill(5)(springs).mkString("?"), Iterator.fill(5)(counts.iterator).flatten.toList)

  type CacheKey = (Set[Spring], List[Spring], List[Int])
  given Cache[CacheKey, Long] = Cache.empty

  def arrangements: Long =
    def helper(allowed: Set[Spring], springs: List[Spring], counts: List[Int]): Memoized[CacheKey, Long] =
      if springs.isEmpty then
        if counts.isEmpty then
          1
        else
          0
      else if !allowed.contains(springs.head) then
        0
      else springs.head match
        case Damaged =>
          if counts.isEmpty then
            0
          else
            if counts.head == 1 then
              Memoize((Set(Operational, Unknown), springs.tail, counts.tail), helper(Set(Operational, Unknown), springs.tail, counts.tail))
            else
              Memoize((Set(Damaged, Unknown), springs.tail, (counts.head - 1) :: counts.tail), helper(Set(Damaged, Unknown), springs.tail, (counts.head - 1) :: counts.tail))
        case Operational =>
          Memoize((Set(Damaged, Operational, Unknown), springs.tail, counts), helper(Set(Damaged, Operational, Unknown), springs.tail, counts))
        case Unknown =>
          (allowed - Unknown).toList.map(x => Memoize((Set(x), x :: springs.tail, counts), helper(Set(x), x :: springs.tail, counts))).sum
    helper(Set(Damaged, Operational, Unknown), springs.toList.map(charToSpring), counts)

  def charToSpring(char: Char): Spring = char match
    case '#' => Damaged
    case '.' => Operational
    case '?' => Unknown

  def springToChar(spring: Spring): Char = spring match
    case Spring.Damaged     => '#'
    case Spring.Operational => '.'
    case Spring.Unknown     => '?'

given Read[List[Int]] = Read(",")
given Read[Row] = Read(" ")
type I = List[Row]
given Read[I] = Read("\n")

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(rows: I): Long =
    rows.map(_.arrangements).sum

  def part2(rows: I): Long =
    rows.map(_.unfold).map(_.arrangements).sum
