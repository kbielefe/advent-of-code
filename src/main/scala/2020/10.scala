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
    val counts = input.appended(0).sorted.reverse.map(_.toLong).foldLeft(Map.empty[Long, Long]){case (counts, next) =>
      val count = Math.max(1L, counts.getOrElse(next + 1, 0L) + counts.getOrElse(next + 2, 0L) + counts.getOrElse(next + 3, 0L))
      counts + (next -> count)
    }
    counts(0)
  }
}
