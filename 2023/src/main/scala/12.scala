package day12
import parse.{*, given}
import algorithms.{Cache, Memoize, Memoized}

enum Spring derives CanEqual:
  case Damaged, Operational, Unknown

case class Row(springs: String, counts: List[Int] - ","):
  import Spring.*
  def unfold: Row =
    Row(Iterator.fill(5)(springs).mkString("?"), Iterator.fill(5)(counts.iterator).flatten.toList.asInstanceOf[List[Int] - ","])

  type CacheKey = (Set[Spring], List[Spring], List[Int], List[Spring])
  given Cache[CacheKey, Set[List[Spring]]] = Cache.empty

  def arrangements: Set[List[Spring]] =
    def helper(allowed: Set[Spring], springs: List[Spring], counts: List[Int], accum: List[Spring]): Memoized[CacheKey, Set[List[Spring]]] =
      if springs.isEmpty then
        if counts.isEmpty then
          Set(accum.reverse)
        else
          Set.empty
      else if !allowed.contains(springs.head) then
        Set.empty
      else springs.head match
        case Damaged =>
          if counts.isEmpty then
            Set.empty
          else
            if counts.head == 1 then
              Memoize((Set(Operational, Unknown), springs.tail, counts.tail, Damaged :: accum), helper(Set(Operational, Unknown), springs.tail, counts.tail, Damaged :: accum))
            else
              Memoize((Set(Damaged, Unknown), springs.tail, (counts.head - 1) :: counts.tail, Damaged :: accum), helper(Set(Damaged, Unknown), springs.tail, (counts.head - 1) :: counts.tail, Damaged :: accum))
        case Operational =>
          Memoize((Set(Damaged, Operational, Unknown), springs.tail, counts, Operational :: accum), helper(Set(Damaged, Operational, Unknown), springs.tail, counts, Operational :: accum))
        case Unknown =>
          (allowed - Unknown).flatMap(x => Memoize((Set(x), x :: springs.tail, counts, accum), helper(Set(x), x :: springs.tail, counts, accum)))
    helper(Set(Damaged, Operational, Unknown), springs.toList.map(charToSpring), counts, List.empty)

  def charToSpring(char: Char): Spring = char match
    case '#' => Damaged
    case '.' => Operational
    case '?' => Unknown

given Read[Row] = Read("(.*) (.*)".r)
type I = List[Row] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(rows: I): Int =
    rows.map(_.arrangements.size).sum

  def part2(rows: I): Int =
    rows.map(_.unfold).map(_.arrangements.size).sum

  def springToChar(spring: Spring): Char = spring match
    case Spring.Damaged     => '#'
    case Spring.Operational => '.'
    case Spring.Unknown     => '?'
