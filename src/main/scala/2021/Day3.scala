package advent2021
import puzzleparse.{*, given}

object Day3:
  def part1(input: List[String]): Int =
    val bits = for
      pos  <- 0 until input.head.length
      line <- input
    yield (pos, line(pos).asDigit)
    val majority = input.size / 2
    val frequency = bits.groupMapReduce(_._1)(_._2)(_ + _)
    val gammaString = (0 until input.head.length).map(pos => if frequency(pos) >= majority then '1' else '0').mkString
    val epsilonString = (0 until input.head.length).map(pos => if frequency(pos) >= majority then '0' else '1').mkString
    val gamma = Integer.parseInt(gammaString, 2)
    val epsilon = Integer.parseInt(epsilonString, 2)
    epsilon * gamma

  def part2(input: List[String]): Int =
    oxygenRating(input) * scrubberRating(input)

  private def oxygenRating(input: List[String], pos: Int = 0): Int =
    if input.size == 1 then
      Integer.parseInt(input.head, 2)
    else
      val oneCount = input.map(_(pos).asDigit).sum
      val zeroCount = input.size - oneCount
      if oneCount >= zeroCount then
        oxygenRating(input.filter(_(pos) == '1'), pos + 1)
      else
        oxygenRating(input.filter(_(pos) == '0'), pos + 1)

  private def scrubberRating(input: List[String], pos: Int = 0): Int =
    if input.size == 1 then
      Integer.parseInt(input.head, 2)
    else
      val oneCount = input.map(_(pos).asDigit).sum
      val zeroCount = input.size - oneCount
      if oneCount >= zeroCount then
        scrubberRating(input.filter(_(pos) == '0'), pos + 1)
      else
        scrubberRating(input.filter(_(pos) == '1'), pos + 1)
