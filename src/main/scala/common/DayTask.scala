package common
import monix.eval.Task
import monix.reactive.Observable

trait DayTask[A, B, C] {
  def input(lines: Observable[String]): Task[A]
  def part1(input: A): Task[B]
  def part2(input: A): Task[C]
}
