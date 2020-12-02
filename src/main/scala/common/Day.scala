package common

import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import outwatch.VDomModifier

abstract class Day[I, A, B](val year: Int, val day: Int) {
  def input(string: String): I
  def part1(input: I, visQueue: VisQueue): Task[A]
  def part2(input: I, visQueue: VisQueue): Task[B]
}

abstract class IntsDay[A, B](year: Int, day: Int) extends Day[Observable[Int], A, B](year, day) {
  override def input(string: String): Observable[Int] =
    Observable.fromIterator(Task(string.linesIterator)).filter(!_.isEmpty).map(_.toInt)
}

abstract class StringsDay[A, B](year: Int, day: Int) extends Day[Observable[String], A, B](year, day) {
  override def input(string: String): Observable[String] =
    Observable.fromIterator(Task(string.linesIterator)).filter(!_.isEmpty)
}
