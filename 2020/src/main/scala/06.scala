package advent2020

import cats.implicits._
import common._
import monix.eval.Task
import monix.reactive.Observable

object Day6 extends MultilineStringsDay[Int, Int](2020, 6) {
  override def part1(input: Observable[Seq[String]]): Task[Int] =
    input.map(_.map(_.toSet).reduce(_ ++ _).size).sumL

  override def part2(input: Observable[Seq[String]]): Task[Int] =
    input.map(_.map(_.toSet).reduce(_ & _).size).sumL
}
