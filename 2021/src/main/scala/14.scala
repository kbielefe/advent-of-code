package advent2021

object Day14:
  def part1(input: List[String]): Long =
    answer(input, 10)

  def part2(input: List[String]): Long =
    answer(input, 40)

  private def answer(input: List[String], iterations: Int): Long =
    val rules = getRules(input)
    val pairs = getPairs(input)
    val lastPairs = Iterator.iterate(pairs)(insertion(rules)).drop(iterations).next
    val individuals = getIndividuals(input.head.head, lastPairs)
    individuals.max - individuals.min

  private def getIndividuals(firstElement: Char, pairs: Map[String, Long]): Iterable[Long] =
    val individualCounts = pairs.toList.map((pair, count) => (pair.last, count))
      .groupMapReduce(_._1)(_._2)(_ + _)
    (individualCounts + (firstElement -> individualCounts.get(firstElement).map(_ + 1).getOrElse(1L))).values

  def insertion(rules: Map[String, Char])(pairs: Map[String, Long]): Map[String, Long] =
    pairs.toList.flatMap((pair, count) => List((pair.head.toString + rules(pair), count), (rules(pair) + pair.last.toString, count)))
      .groupMapReduce(_._1)(_._2)(_ + _)

  private def getRules(input: List[String]): Map[String, Char] =
    input.drop(1).map(_.split(" -> ")).map{case Array(x, y) => (x, y.head)}.toMap

  private def getPairs(input: List[String]): Map[String, Long] =
    input.head.sliding(2).toList.map(_.mkString).groupMapReduce(identity)(_ => 1L)(_ + _)
