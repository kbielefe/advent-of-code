package advent2016

object Day8:
  private val rect         = """rect (\d+)x(\d+)""".r
  private val rotateColumn = """rotate column x=(\d+) by (\d+)""".r
  private val rotateRow    = """rotate row y=(\d+) by (\d+)""".r

  def part1(input: List[String]): Int =
    pixels(input).size

  def part2(input: List[String]): String =
    val output = pixels(input)
    (0 until 6).map{y =>
      (0 until 59).map{x =>
        if output.contains((x, y)) then '█' else ' '
      }.mkString
    }.mkString("\n")

  def pixels(input: List[String]): Set[(Int, Int)] =
    input.foldLeft(Set.empty[(Int, Int)]){
      case (set, rect(width, height)) =>
        val newPixels = for
          x <- 0 until width.toInt
          y <- 0 until height.toInt
        yield (x, y)
        set ++ newPixels
      case (set, rotateColumn(x, count)) =>
        val column = set.filter(_._1 == x.toInt)
        val newColumn = column.map{case (x, y) => (x, (y + count.toInt) % 6)}
        set -- column ++ newColumn
      case (set, rotateRow(y, count)) =>
        val row = set.filter(_._2 == y.toInt)
        val newRow = row.map{case (x, y) => ((x + count.toInt) % 50, y)}
        set -- row ++ newRow
    }
