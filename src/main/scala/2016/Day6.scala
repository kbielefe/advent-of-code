package advent2016

object Day6:
  def part1(input: List[String]): String =
    input.transpose.map(mostFrequent).mkString

  def part2(input: List[String]): String =
    input.transpose.map(leastFrequent).mkString

  private def mostFrequent(input: List[Char]): Char =
    input.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)._1

  private def leastFrequent(input: List[Char]): Char =
    input.groupMapReduce(identity)(_ => 1)(_ + _).minBy(_._2)._1
