package day17
import parse.{*, given}
import algorithms.*

type I = List[Int] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    exactFitCombinationCount(150, input)

  def part2(input: I): Int =
    minimumCombinationCount(150, input)

  def exactFitCombinationCount(amount: Int, containers: List[Int]): Int =
    (1 to containers.size).iterator.flatMap(n => containers.combinationsWithRepetitions(n).map(_.sum)).count(_ == amount)

  def minimumCombinationCount(amount: Int, containers: List[Int]): Int =
    val minSize = (1 to containers.size).iterator.flatMap(n => containers.combinationsWithRepetitions(n)).find(_.sum == amount).get.size
    containers.combinationsWithRepetitions(minSize).count(_.sum == amount)
