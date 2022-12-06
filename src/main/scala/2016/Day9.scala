package advent2016

object Day9:
  def part1(input: String): Long =
    decompress(input.trim.iterator)

  def part2(input: String): Long =
    ???

  private val markerRegex = """(\d+)x(\d+)""".r

  private def decompress(input: Iterator[Char]): Long =
    if input.hasNext then
      val (raw, markerPlus) = input.span(_ != '(')
      val (marker, remainder) = markerPlus.span(_ != ')')
      val markerRegex(count, repeat) = marker.drop(1).mkString: @unchecked
      raw.size + (repeat.toLong * count.toLong) + decompress(remainder.drop(1 + count.toInt))
    else
      0
