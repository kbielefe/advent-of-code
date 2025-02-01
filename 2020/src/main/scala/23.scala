package day23

import algorithms.{Circular, digits}
import parse.{*, given}
import scala.annotation.tailrec

object Puzzle extends runner.Day[Int, Long, Long]:
  def part1(input: Int): Long =
    val circular = Circular(input.digits)
    move(circular, input.digits.head, 100, input.digits.max)
    Iterator.iterate(circular.next(1))(circular.next).takeWhile(_ != 1).mkString.toLong

  def part2(input: Int): Long =
    val circular = Circular(input.digits.iterator ++ Iterator.range(10, 1000001))
    move(circular, input.digits.head, 10000000, 1000000)
    val star1 = circular.next(1)
    val star2 = circular.next(star1)
    star1.toLong * star2.toLong

  @tailrec
  def move(circular: Circular[Int], current: Int, count: Int, max: Int): Unit =
    if count == 0 then
      return
    val removed = circular.removeNAfter(3, current)
    val destination = (Iterator.range(current - 1, 0, -1) ++ Iterator.range(max, current, -1)).dropWhile(removed.contains).next
    circular.insertAfter(removed, destination)
    move(circular, circular.next(current), count - 1, max)
