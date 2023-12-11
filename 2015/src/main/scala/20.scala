package day20
import algorithms.*
import parse.{*, given}

object Puzzle extends runner.Day[Int, Int, Int]:
  def part1(input: Int): Int =
    Iterator.from(1).find(house => house.allFactors.sum * 10 >= input).get

  def part2(input: Int): Int =
    Iterator.from(1).find(house => house.allFactors.filter(factor => factor * 50 >= house).sum * 11 >= input).get
