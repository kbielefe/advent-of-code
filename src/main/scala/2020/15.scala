package advent2020

import common._

object Day15 extends CommaSeparatedIntsDay[Int, Int](2020, 15) {
  private def answer(input: Seq[Int], target: Int): Int = {
    val lastTurns = input.init.zipWithIndex.map{case (number, turn) => (number, turn + 1)}.toMap
    val turn = input.size
    val lastNumber = input.last
    val turns = Iterator.iterate((lastTurns, lastNumber, turn)){case (lastTurns, lastNumber, turn) =>
      val nextNumber = lastTurns.get(lastNumber).map(turn - _).getOrElse(0)
      (lastTurns + (lastNumber -> turn), nextNumber, turn + 1)
    }
    turns.dropWhile(_._3 < target).next()._2
  }

  override def part1(input: Seq[Int]): Int = answer(input, 2020)
  override def part2(input: Seq[Int]): Int = answer(input, 30000000)
}
