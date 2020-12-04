package advent2019

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day1 extends IntsDay[Int, Int](2019, 1) {
  private def compound(in: Int): Task[Int] =
    Observable.unfold(in){mass =>
      val fuel = mass / 3 - 2
      if (fuel > 0) Some(fuel -> fuel) else None
    }.sumL

  override def part1(input: Observable[Int]): Task[Int] = input.map(_ / 3 - 2).sumL
  override def part2(input: Observable[Int]): Task[Int] = input.mapEval(compound).sumL
}
