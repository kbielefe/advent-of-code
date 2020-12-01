package advent2020

import common.IntsDay
import monix.eval.Task
import monix.reactive.Observable

object Day1 extends IntsDay[Int, Int](2020, 1) {
  override def part1(input: Observable[Int]): Task[Int] = input.toListL.map{list =>
    val expenses = list.toSet
    expenses
      .find(expense => expenses.contains(2020 - expense))
      .map(expense => expense * (2020 - expense))
      .get
  }

  override def part2(input: Observable[Int]): Task[Int] = input.toListL.map{list =>
    val expenses = list.toSet
    list
      .combinations(2)
      .filter{case List(x, y) => x != y}
      .find{case List(x, y) => expenses.contains(2020 - x - y)}
      .map{case List(x, y) => x * y * (2020 - x - y)}
      .get
  }
}
