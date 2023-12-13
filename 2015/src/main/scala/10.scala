package day10
import parse.{*, given}
import algorithms.*

object Puzzle extends runner.Day[String, Int, Int]:
  def part1(input: String): Int =
    Iterator.iterate(input.map(_.asDigit).iterator)(lookAndSay).drop(40).size

  def part2(input: String): Int =
    ???

  def lookAndSay(in: Iterator[Int]): Iterator[Int] =
    in.group.flatMap{group =>
      val list = group.toList
      Iterator(list.size, list.head)
    }
