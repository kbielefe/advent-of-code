package advent2015

object Day8:
  def part1(input: List[String]): Int =
    input.map{string =>
      string.size - string.drop(1).dropRight(1).replaceAll("""\\\\|\\"|\\x..""", ".").size
    }.sum

  def part2(input: List[String]): Int =
    input.map{string =>
      string.replaceAll("\"|\\\\", "\\\\.").size - string.size + 2
    }.sum
