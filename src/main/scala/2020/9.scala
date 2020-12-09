package advent2020

import common._

object Day9 extends LongDay[Long, Long](2020, 9) {
  private def valid(seq: Seq[Long]): Boolean = {
    val last = seq.last
    seq.init.combinations(2).exists{_.sum == last}
  }

  @scala.annotation.tailrec
  private def encryptionWeakness(from: Int, until: Int, seq: Seq[Long], invalidNumber: Long): Long = {
    val slice = seq.slice(from, until)
    val sum = slice.sum
    if (sum == invalidNumber)
      slice.min + slice.max
    else if (sum > invalidNumber)
      encryptionWeakness(from + 1, until, seq, invalidNumber)
    else
      encryptionWeakness(from, until + 1, seq, invalidNumber)
  }

  override def part1(input: Seq[Long]): Long =
    input.sliding(26).find(seq => !valid(seq)).get.last

  override def part2(input: Seq[Long]): Long =
    encryptionWeakness(0, 1, input, part1(input))
}
