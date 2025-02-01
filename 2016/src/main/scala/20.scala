package advent2016
import scala.annotation.tailrec

object Day20:
  def part1(input: List[String]): Long =
    lowestUnblocked(0, blacklist(input))

  def part2(input: List[String]): Long =
    unblockedCount(0, blacklist(input), 0)

  private def blacklist(input: List[String]): List[(Long, Long)] =
    input.collect{case s"${start}-${end}" => (start.toLong, end.toLong)}.sorted

  @tailrec
  private def lowestUnblocked(max: Long, blacklist: List[(Long, Long)]): Long =
    val (start, end) = blacklist.head
    if max + 1 < start then
      max + 1
    else
      lowestUnblocked(Math.max(max, end), blacklist.tail)

  @tailrec
  private def unblockedCount(max: Long, blacklist: List[(Long, Long)], count: Long): Long =
    if blacklist == Nil then
      count + (if max < 4294967295L then 4294967295L - max - 1 else 0)
    else
      val (start, end) = blacklist.head
      if max + 1 < start then
        unblockedCount(end, blacklist.tail, count + start - max - 1)
      else
        unblockedCount(Math.max(max, end), blacklist.tail, count)
