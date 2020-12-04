package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable
import outwatch.VDomModifier
import outwatch.dsl._

object Day1 extends IntsDay[Int, Int](2020, 1) {
  private def sumsTo2020(input: List[Int], count: Int): List[Int] = {
    val expenses = input.toSet
    input
      .combinations(count - 1)
      .find(combo => expenses.contains(2020 - combo.sum))
      .map(combo => (2020 - combo.sum) :: combo)
      .get
  }

  override def part1(input: Observable[Int]): Task[Int] =
    input.toListL.map(sumsTo2020(_, 2).product)

  override def part2(input: Observable[Int]): Task[Int] =
    input.toListL.map(sumsTo2020(_, 3).product)

  override def vis1(input: Observable[Int]): Observable[VDomModifier] =
    Observable.fromTask(input.toListL).map(sumsTo2020(_, 2)).map(_.mkString(" * "))
}
