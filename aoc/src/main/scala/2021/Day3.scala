package advent2021
import puzzleparse.{*, given}

object Day3:
  def part1(input: List[String]): Int =
    val gamma = (0 until input.head.length).foldLeft(0){case (acc, bit) => (acc << 1) + mostFrequent(input, bit)}
    val epsilon = ~gamma & ((1 << input.head.length) - 1)
    epsilon * gamma

  def part2(input: List[String]): Int =
    rating(input, _ == _) * rating(input, _ != _)

  private def mostFrequent(input: List[String], bit: Int): Int =
    val oneCount = input.map(_(bit).asDigit).sum
    val zeroCount = input.size - oneCount
    if oneCount >= zeroCount then 1 else 0

  def rating(input: List[String], compare: (Int, Int) => Boolean): Int =
    val result = (0 until input.head.length)
      .iterator
      .scanLeft(input){case (list, bit) =>
        list.filter(num => compare(num(bit).asDigit, mostFrequent(list, bit)))
      }
      .dropWhile(_.size > 1)
    Integer.parseInt(result.next.head, 2)
