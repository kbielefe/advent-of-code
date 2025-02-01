package advent2020

import common._

object Day13 extends SyncStringsDay[Int, BigInt](2020, 13) {
  override def part1(input: Seq[String]): Int = {
    val earliest = input(0).toInt
    val busIDs = input(1).split(",").filterNot(_ == "x").map(_.toInt)
    val (earliestBus, shortestWait) = busIDs.map(id => (id, id - (earliest % id))).minBy(_._2)
    earliestBus * shortestWait
  }

  override def part2(input: Seq[String]): BigInt = {
    val busIDs = input(1)
      .split(",")
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map{case (id, index) => (BigInt(id), BigInt(index))}
      .map{case (id, index) => (id - index, id)}

    val remainder = busIDs.reduceLeft[(BigInt, BigInt)]{case ((combinedA, combinedN), (nextA, nextN)) =>
      chineseRemainder(combinedA, combinedN, nextA, nextN)
    }
    remainder._1
  }

  private def chineseRemainder(a1: BigInt, n1: BigInt, a2: BigInt, n2: BigInt): (BigInt, BigInt) = {
    println(a1, n1, a2, n2)
    val (m1, m2) = bézout(n1, n2)
    normalize(a1 * m2 * n2 + a2 * m1 * n1, n1 * n2)
  }

  private def bézout(a: BigInt, b: BigInt): (BigInt, BigInt) = {
    def helper(oldR: BigInt, r: BigInt, oldS: BigInt, s: BigInt, oldT: BigInt, t: BigInt): (BigInt, BigInt) =
      if (r == 0)
        (oldS, oldT)
      else {
        val quotient = oldR / r
        helper(r, oldR - quotient * r, s, oldS - quotient * s, t, oldT - quotient * t)
      }
    helper(a, b, 1, 0, 0, 1)
  }

  private def normalize(a: BigInt, n: BigInt): (BigInt, BigInt) = {
    val newA = a % n
    if (newA < 0)
      (newA + n, n)
    else
      (newA, n)
  }
}
