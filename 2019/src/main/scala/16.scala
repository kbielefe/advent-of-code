package day16

import parse.given

object Puzzle extends runner.Day[String, String, String]:
  def part1(input: String): String =
    ???

  def part2(input: String): String =
    val offset = input.take(7).mkString.toInt
    val initial = Iterator.range(0, 10000).flatMap(_ => Iterator.from(input.map(_.asDigit))).drop(offset).toSeq.reverse
    Iterator.iterate(initial)(fft).drop(100).next.reverse.take(8).mkString

  def fft(input: Seq[Int]): Seq[Int] =
    input.scanLeft(0)((x, y) => (x + y) % 10).drop(1)
