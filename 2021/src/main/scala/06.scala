package advent2021
import puzzleparse.DList
import scala.annotation.tailrec

object Day6:
  def part1(input: DList[",", Int]): Long = count(80,  initialCounts(input))
  def part2(input: DList[",", Int]): Long = count(256, initialCounts(input))

  private def initialCounts(input: List[Int]): Map[Int, Long] =
    input.groupMapReduce(identity)(_ => 1L)(_ + _)

  @tailrec
  private def count(days: Int, counts: Map[Int, Long]): Long =
    if days == 0 then
      counts.values.sum
    else
      val newCounts = counts.flatMap{
        case (0 -> c) => Map(6 -> (c + counts.getOrElse(7, 0L)), 8 -> c)
        case (7 -> c) => Map(6 -> (c + counts.getOrElse(0, 0L)))
        case (n -> c) => Map(n - 1 -> c)
      }
      count(days - 1, newCounts)
