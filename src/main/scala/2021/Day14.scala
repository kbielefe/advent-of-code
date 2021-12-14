package advent2021

object Day14:
  def part1(input: List[String]): Long =
    answer(input, 10)

  def part2(input: List[String]): Long =
    answer(input, 40)

  private def answer(input: List[String], iterations: Int): Long =
    Iterator.iterate(Counts(input))(_.insertion).drop(iterations).next.answer

  case class Counts(rules: Map[String, Char], pair: Map[String, Long], individual: Map[Char, Long]):
    def answer: Long = individual.values.max - individual.values.min

    def insertion: Counts =
      val newPair = pair.toList
        .flatMap((pair, count) => List((pair.head.toString + rules(pair), count), (rules(pair) + pair.last.toString, count)))
        .groupMapReduce(_._1)(_._2)(_ + _)
      val newIndividualFromPairs = pair.toList
        .map((pair, count) => (rules(pair), count))
        .groupMapReduce(_._1)(_._2)(_ + _)
      val newIndividual = (newIndividualFromPairs.keySet ++ individual.keySet)
        .map(key => (key, newIndividualFromPairs.getOrElse(key, 0L) + individual.getOrElse(key, 0L)))
        .toMap
      Counts(rules, newPair, newIndividual)

  object Counts:
    def apply(input: List[String]): Counts =
      val rules = input.drop(1).map(_.split(" -> ")).map{case Array(x, y) => (x, y.head)}.toMap
      val pair = input.head.sliding(2).toList.map(_.mkString).groupMapReduce(identity)(_ => 1L)(_ + _)
      val individual = input.head.groupMapReduce(identity)(_ => 1L)(_ + _)
      Counts(rules, pair, individual)
