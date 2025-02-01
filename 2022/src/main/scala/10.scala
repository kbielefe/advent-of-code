package advent2022

object Day10:
  def part1(input: List[String]): Int =
    xs(input)
      .zipWithIndex
      .map{(x, index) => (x, index + 1)}
      .filter(_._2 % 40 == 20)
      .map{(x, cycle) => x * cycle}
      .sum

  def part2(input: List[String]): String =
    val pixels = xs(input).zipWithIndex.map{(spritePos, pixel) =>
      if Math.abs((pixel % 40) - spritePos) <= 1 then '█' else ' '
    }
    pixels.grouped(40).map(_.mkString).mkString("\n")

  private def xs(input: List[String]): List[Int] =
    val expanded = input.flatMap{
      case "noop" => List("noop")
      case x      => List("noop", x)
    }
    expanded.scanLeft(1){
      case (x, "noop")     => x
      case (x, s"addx $y") => x + y.toInt
    }
