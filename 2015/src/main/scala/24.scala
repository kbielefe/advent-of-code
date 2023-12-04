package day24

import parse.{*, given}

type I = List[Long] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long = answer(input, 3)

  def part2(input: I): Long = answer(input, 4)

  def answer(input: I, groupCount: Long): Long =
    val groupSize = input.sum / groupCount
    val minSize = (1 to input.size).iterator.flatMap(input.combinations).find(_.sum == groupSize).get.size
    input.combinations(minSize).filter(_.sum == groupSize).map(quantumEntanglement).min

  def quantumEntanglement(packages: List[Long]): Long =
    packages.product
