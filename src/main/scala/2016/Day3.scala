package advent2016

object Day3:
  def part1(input: List[List[Int]]): Int =
    input.count(valid)

  def part2(input: List[List[Int]]): Int =
    input.grouped(3).flatMap(_.transpose).count(valid)

  private def valid(triangle: List[Int]): Boolean =
    triangle.permutations.forall(x => x(0) + x(1) > x(2))
