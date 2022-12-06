package advent2016

object Day9:
  def part1(input: String): Int =
    decompress(input.trim.iterator).size

  def part2(input: String): Int =
    ???

  private val markerRegex = """(\d+)x(\d+)""".r

  private def decompress(input: Iterator[Char]): Iterator[Char] =
    if input.hasNext then
      val (raw, markerPlus) = input.span(_ != '(')
      val (marker, remainder) = markerPlus.span(_ != ')')
      val markerRegex(count, repeat) = marker.drop(1).mkString: @unchecked
      val (repeated, last) = remainder.drop(1).duplicate
      raw ++ repeatIter(repeat.toInt, repeated.take(count.toInt)) ++ decompress(last.drop(count.toInt))
    else
      Iterator.empty

  private def repeatIter(count: Int, input: Iterator[Char]): Iterator[Char] =
    if count == 0 then
      Iterator.empty
    else
      val (current, next) = input.duplicate
      current ++ repeatIter(count - 1, next)
