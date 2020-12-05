package advent2020

import cats.implicits._
import common._
import monix.eval.Task
import monix.reactive.Observable

object Day5 extends StringsDay[Int, Int](2020, 5) {
  private def seatId(string: String): Int = {
    val binary = string.map{
      case 'L' => '0'
      case 'R' => '1'
      case 'F' => '0'
      case 'B' => '1'
    }
    Integer.parseInt(binary, 2)
  }

  override def part1(input: Observable[String]): Task[Int] =
    input.map(seatId).maxL.map(_.get)

  override def part2(input: Observable[String]): Task[Int] =
    input.map(seatId).toListL.map(_.toSet).map{seats =>
      val min = seats.min
      val max = seats.max
      ((min to max).toSet -- seats).head
    }
}
