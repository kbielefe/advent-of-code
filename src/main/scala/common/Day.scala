package common

import monix.eval.Task
import monix.reactive.Observable

abstract class Day[I, A, B](val year: Int, val day: Int) {
  def input(string: String): I
  def part1(input: I): Task[A]
  def part2(input: I): Task[B]
}

abstract class IntsDay[A, B](year: Int, day: Int) extends Day[Observable[Int], A, B](year, day) {
  override def input(string: String): Observable[Int] =
    Observable.fromIterator(Task(string.linesIterator)).filter(!_.isEmpty).map(_.toInt)
}
