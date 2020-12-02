package advent2020

import common.{IntsDay, StringVis, VisQueue}
import monix.eval.Task
import monix.reactive.Observable

object Day1 extends IntsDay[Int, Int](2020, 1) {
  override def part1(input: Observable[Int], visQueue: VisQueue): Task[Int] = input.toListL.flatMap{list =>
    val expenses = list.toSet
    val pair = expenses
      .find(expense => expenses.contains(2020 - expense))
      .map(expense => (expense, (2020 - expense)))
      .get
    StringVis.set(visQueue, pair.toString) *>
    Task(pair._1 * pair._2)
  }

  override def part2(input: Observable[Int], visQueue: VisQueue): Task[Int] = input.toListL.flatMap{list =>
    val expenses = list.toSet
    val triple = list
      .combinations(2)
      .filter{case List(x, y) => x != y}
      .find{case List(x, y) => expenses.contains(2020 - x - y)}
      .map{case List(x, y) => (x, y, (2020 - x - y))}
      .get
    StringVis.set(visQueue, triple.toString) *>
    Task(triple._1 * triple._2 * triple._3)
  }
}
