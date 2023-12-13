package day12
import parse.{*, given}
import algorithms.{Cache, Memoize, Memoized}

enum Spring derives CanEqual:
  case Damaged, Operational, Unknown

case class Row(springs: String, counts: List[Int] - ","):
  import Spring.*
  def unfold: Row =
    Row(Iterator.fill(5)(springs).mkString("?"), Iterator.fill(5)(counts.iterator).flatten.toList.asInstanceOf[List[Int] - ","])

  type CacheKey = (Set[Spring], List[Spring], List[Int])
  given Cache[CacheKey, Int] = Cache.empty

  def arrangements: Int =
    def helper(allowed: Set[Spring], springs: List[Spring], counts: List[Int]): Memoized[CacheKey, Int] =
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
              helper(Set(Operational, Unknown), springs.tail, counts.tail)
            else
              helper(Set(Damaged, Unknown), springs.tail, (counts.head - 1) :: counts.tail)
        case Operational =>
          helper(Set(Damaged, Operational, Unknown), springs.tail, counts)
        case Unknown =>
          (allowed - Unknown).map(x => helper(Set(x), x :: springs.tail, counts)).sum
    helper(Set(Damaged, Operational, Unknown), springs.toList.map(charToSpring), counts)

  def charToSpring(char: Char): Spring = char match
    case '#' => Damaged
    case '.' => Operational
    case '?' => Unknown

  def springToChar(spring: Spring): Char = spring match
    case Spring.Damaged     => '#'
    case Spring.Operational => '.'
    case Spring.Unknown     => '?'

given Read[Row] = Read("(.*) (.*)".r)
type I = List[Row] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(rows: I): Int =
    rows.drop(1).take(1).map(_.arrangements).foreach(println)
    //rows.map(_.arrangements).sum
    ???

  def part2(rows: I): Int =
    rows.map(_.unfold).map(_.arrangements).sum
