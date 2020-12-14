package advent2020

import common._

object Day13 extends SyncStringsDay[Int, Long](2020, 13) {
  override def part1(input: Seq[String]): Int = {
    val earliest = input(0).toInt
    val busIDs = input(1).split(",").filterNot(_ == "x").map(_.toInt)
    val (earliestBus, shortestWait) = busIDs.map(id => (id, id - (earliest % id))).minBy(_._2)
    earliestBus * shortestWait
  }

  override def part2(input: Seq[String]): Long = {
    val busIDs = input(1).split(",").zipWithIndex.filterNot(_._1 == "x").map{case (id, index) => (id.toLong - index.toLong, id.toLong)}
    val remainder = busIDs.reduceLeft[(Long, Long)]{case ((combinedA, combinedN), (nextA, nextN)) =>
      chineseRemainder(combinedA, combinedN, nextA, nextN)
    }
    remainder._1
  }
  // 765,542,694,402,353 is too high

  private def chineseRemainder(a1: Long, n1: Long, a2: Long, n2: Long): (Long, Long) = {
    val (m1, m2) = bézout(n1, n2)
    normalize(a1 * m2 * n2 + a2 * m1 * n1, n1 * n2)
  }

  private def bézout(a: Long, b: Long): (Long, Long) = {
    def helper(oldR: Long, r: Long, oldS: Long, s: Long, oldT: Long, t: Long): (Long, Long) =
      if (r == 0)
        (oldS, oldT)
      else {
        val quotient = oldR / r
        helper(r, oldR - quotient * r, s, oldS - quotient * s, t, oldT - quotient * t)
      }
    helper(a, b, 1, 0, 0, 1)
  }

  private def normalize(a: Long, n: Long): (Long, Long) = {
    val newA = a % n
    if (newA < 0)
      (newA + n, n)
    else
      (newA, n)
  }
}
