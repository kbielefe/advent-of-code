package advent2021
import puzzleparse.Letters

object Day8:
  def part1(input: List[List[Letters]]): Int =
    input.flatMap(_.drop(11)).map(_.size).count(Set(2,3,4,7) contains _)

  def part2(input: List[List[Letters]]): Int =
    input.map(unscramble).sum

  private def unscramble(input: List[Letters]): Int =
    val inputs  = input.take(10)
    val outputs = input.drop(11)
    val one  = hasSize(inputs, 2)
    val four = hasSize(inputs, 4)
    outputs.map(segmentsToDigit(one, four)).foldLeft(0)(_ * 10 + _)

  private def hasSize(inputs: List[Letters], size: Int): Set[Char] =
    inputs.find(_.size == size).get.toSet

  private def overlapCount(other: Set[Char], segments: String): Int =
    (segments.toSet & other).size

  private def segmentsToDigit(one: Set[Char], four: Set[Char])(segments: String): Int =
    (segments.size, overlapCount(one, segments), overlapCount(four, segments)) match
      case (6, 2, 3) => 0
      case (2, _, _) => 1
      case (5, 1, 2) => 2
      case (5, 2, 3) => 3
      case (4, _, _) => 4
      case (5, 1, 3) => 5
      case (6, 1, 3) => 6
      case (3, _, _) => 7
      case (7, _, _) => 8
      case (6, 2, 4) => 9
