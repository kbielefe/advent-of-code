package advent2022

object Day6:
  def part1(input: String): Int =
    answer(input, 4)

  def part2(input: String): Int =
    answer(input, 14)

  def answer(input: String, size: Int): Int =
    input.trim.sliding(size).zipWithIndex.find(_._1.toSet.size == size).get._2 + size
