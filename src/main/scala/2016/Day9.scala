package advent2016

object Day9:
  def part1(input: String): Long =
    decompress(input.trim, 1)

  def part2(input: String): Long =
    decompress(input.trim, 2)

  private val markerRegex = """(\d+)x(\d+)""".r

  private def decompress(input: String, version: Int): Long =
    if input.isEmpty then
      0
    else if input.head == '(' then
      val marker = input.takeWhile(_ != ')').drop(1)
      val remainder = input.dropWhile(_ != ')').drop(1)
      val markerRegex(count, repeat) = marker: @unchecked
      val repeated = if version == 1 then count.toLong else decompress(remainder.take(count.toInt), version)
      repeat.toLong * repeated + decompress(remainder.drop(count.toInt), version)
    else
      1 + decompress(input.tail, version)
