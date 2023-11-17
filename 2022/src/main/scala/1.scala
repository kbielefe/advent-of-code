import parse.{*, given}

type Day1Input = List[List[Int] - "\n"] - "\n\n"
object Day1 extends runner.Day[Day1Input, Int, Int]:
  def part1(input: Day1Input): Int =
    input.map(_.sum).max

  def part2(input: Day1Input): Int =
    input.map(_.sum).sorted.reverse.take(3).sum
