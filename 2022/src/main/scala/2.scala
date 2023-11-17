import parse.{*, given}

case class Round(opponent: Char, me: Char) derives ReadProduct

type Day2Input = List[Round - " "] - "\n"
object Day2 extends runner.Day[Day2Input, Int, Int]:
  def part1(input: Day2Input): Int =
    input.foreach(println)
    15

  def part2(input: Day2Input): Int =
    ???
