package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day1 extends IntsDay[Int, Int](2020, 1) {
  private def sumsTo2020(input: List[Int], count: Int, visQueue: VisQueue): Task[Int] = {
    val expenses = input.toSet
    val list = input
      .combinations(count - 1)
      .find(combo => expenses.contains(2020 - combo.sum))
      .map(combo => (2020 - combo.sum) :: combo)
      .get
    visQueue.set(list.sorted.mkString(" * ")) *>
    Task(list.product)
  }

  override def part1(input: Observable[Int], visQueue: VisQueue): Task[Int] =
    input.toListL.flatMap(sumsTo2020(_, 2, visQueue))

  override def part2(input: Observable[Int], visQueue: VisQueue): Task[Int] =
    input.toListL.flatMap(sumsTo2020(_, 3, visQueue))
}
