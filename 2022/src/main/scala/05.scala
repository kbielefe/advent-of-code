package advent2022

object Day5:
  private val move = """move (\d+) from (\d+) to (\d+)""".r

  private def stacks(input: String): Map[Int, List[Char]] =
    input
      .split("\n")
      .toList
      .takeWhile(_.contains("["))
      .transpose
      .filter(_.exists(x => x >= 'A' && x <= 'Z'))
      .map(_.filter(_ != ' '))
      .zipWithIndex
      .map{case (stack, index) => (index + 1, stack)}
      .toMap

  private def moves(input: String): Array[String] =
    input
      .split("\n")
      .dropWhile(!_.contains("move"))

  private def endStacks(input: String, reverse: Boolean): Map[Int, List[Char]] =
    moves(input).foldLeft(stacks(input)){
      case (stacks, move(count, from, to)) =>
        val fromStack = stacks(from.toInt)
        val toStack = stacks(to.toInt)
        val moved = fromStack.take(count.toInt)
        val newFrom = fromStack.drop(count.toInt)
        val newTo = if reverse then moved.reverse ++ toStack else moved ++ toStack
        stacks + (from.toInt -> newFrom) + (to.toInt -> newTo)
    }


  private def answer(stacks: Map[Int, List[Char]]): String =
    stacks.toList.sortBy(_._1).map(_._2.head).mkString

  def part1(input: String): String =
    answer(endStacks(input, true))

  def part2(input: String): String =
    answer(endStacks(input, false))
