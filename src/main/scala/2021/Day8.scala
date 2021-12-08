package advent2021
import puzzleparse.{*, given}

object Day8:
  def part1(input: List[List[Letters]]): Int =
    input.flatMap(_.drop(10)).map(_.size).count(x => x == 7 || x == 4 || x == 3 || x == 2)

  def part2(input: List[List[Letters]]): Int =
    input.map(unscramble).sum

  private def unscramble(input: List[Letters]): Int =
    val inputs = input.take(10)
    val outputs = input.drop(11) // 11 to remove the pipe
    val one   = hasSize(inputs, 2)
    val four  = hasSize(inputs, 4)
    def findMapping(input: String): (String, Int) =
      def size(other: Set[Char]): Int = (input.toSet & other).size
      val key = (input.size, size(one), size(four))
      (input.sorted -> overlaps(key))
    val mapping = inputs.map(findMapping).toMap
    outputs.map(_.sorted).map(mapping).foldLeft(0)((acc, digit) => acc * 10 + digit)

  private def hasSize(inputs: List[Letters], size: Int): Set[Char] =
    inputs.find(_.size == size).get.toSet

  private val overlaps = Map(
    // (size, overlaps with 1, overlaps with 4)
    (6, 2, 3) -> 0,
    (2, 2, 2) -> 1,
    (5, 1, 2) -> 2,
    (5, 2, 3) -> 3,
    (4, 2, 4) -> 4,
    (5, 1, 3) -> 5,
    (6, 1, 3) -> 6,
    (3, 2, 2) -> 7,
    (7, 2, 4) -> 8,
    (6, 2, 4) -> 9
  )
