package advent2020

import common._

object Day10 extends SyncIntsDay[Int, Long](2020, 10) {
  override def part1(input: Seq[Int]): Int = {
    val intervals = input.sorted.sliding(2).map{case Seq(a, b) => b - a}.toSeq
    val oneJolt   = intervals.count(_ == 1) + 1
    val threeJolt = intervals.count(_ == 3) + 1
    oneJolt * threeJolt
  }

  override def part2(input: Seq[Int]): Long = {
    val init = Map(input.max + 3 -> 1L)
    val counts = input.appended(0).sorted.reverse.foldLeft(init){case (counts, next) =>
      val count = (next + 1 to next + 3).map(counts.getOrElse(_, 0L)).sum
      counts + (next -> count)
    }
    counts(0)
  }
}
