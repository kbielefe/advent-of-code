package day1
import parse.{*, given}

case class Lists(left: List[Int], right: List[Int]):
  def distance: Int =
    left.sorted
      .zip(right.sorted)
      .map{case (left, right) => Math.abs(right - left)}
      .sum

  def similarity: Int =
    val rightCount = right.groupMapReduce(identity)(identity)(_ + _)
    left.map(l => rightCount.getOrElse(l, 0)).sum

given Read[(Int, Int)] = Read("""(\d+)\s+(\d+)""".r)

given Read[List[(Int, Int)]] = Read("\n")

given Read[Lists] = summon[Read[List[(Int, Int)]]].map: tuples =>
  Lists(tuples.map(_._1), tuples.map(_._2))

object Puzzle extends runner.Day[Lists, Int, Int]:
  def part1(input: Lists): Int =
    input.distance

  def part2(input: Lists): Int =
    input.similarity
