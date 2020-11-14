package advent2020

import common.IntsDay
import monix.eval.Task
import monix.reactive.Observable

object Day1 extends IntsDay[Int, Int](2020, 1) {
  override def part1(input: Observable[Int]): Task[Int] = Task(1)
  override def part2(input: Observable[Int]): Task[Int] = Task(2)
}

object Day2 extends IntsDay[Int, Int](2020, 2) {
  override def part1(input: Observable[Int]): Task[Int] = Task(1)
  override def part2(input: Observable[Int]): Task[Int] = Task(2)
}
