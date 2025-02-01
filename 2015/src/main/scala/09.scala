package advent2015

object Day9:
  def part1(input: List[String]): Int = routes(input).min
  def part2(input: List[String]): Int = routes(input).max

  private def routes(input: List[String]): Iterator[Int] =
    val distances = input.flatMap{
      case s"$x to $y = $d" => List((x, y) -> d.toInt, (y, x) -> d.toInt)
    }.toMap
    val destinations = distances.keySet.map(_._1).toList
    destinations.permutations.map(_.sliding(2).map{case List(x, y) => distances(x -> y)}.sum)
