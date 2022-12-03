package advent2022

object Day3:
  def part1(input: List[String]): Int =
    input.map(common).sum

  def part2(input: List[String]): Int =
    input.grouped(3).map(badge).sum

  private def common(input: String): Int =
    val (left, right) = input.splitAt(input.size / 2)
    priority((left.toSet & right.toSet).head)

  private def badge(input: List[String]): Int =
    val first :: second :: third :: Nil = input: @unchecked
    priority((first.toSet & second.toSet & third.toSet).head)

  private def priority(input: Char): Int =
    if input.isLower then
      input - 'a' + 1
    else
      input - 'A' + 27
