package advent2020

import cats.implicits._
import common._
import monix.eval.Task
import monix.reactive.Observable

object Day3 extends GridDay[Int, Long](2020, 3) {
  private def trees(input: Map[(Int, Int), Char])(slope: (Int, Int)): Task[Int] = {
    val (slopeX, slopeY) = slope
    val maxX = input.keySet.map(_._1).max
    Observable.unfold((0, 0)){case key@(x, y) =>
      input.get(key).map{char =>
        (if (char == '#') 1 else 0, ((x + slopeX) % (maxX + 1), y + slopeY))
      }
    }.sumL
  }

  override def part1(input: Map[(Int, Int), Char]): Task[Int] =
    trees(input)(3 -> 1)

  override def part2(input: Map[(Int, Int), Char]): Task[Long] =
    Task.parSequenceUnordered(
      List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(trees(input))
    ).map(_.map(_.toLong).product)
}
