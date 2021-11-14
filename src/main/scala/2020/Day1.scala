package advent2020

object Day1:
  def part1(input: List[Int]): Int = sumsTo2020(input, 2).product

  def part2(input: List[Int]): Int = sumsTo2020(input, 3).product

  private def sumsTo2020(input: List[Int], count: Int): List[Int] =
    val expenses = input.toSet
    input
      .combinations(count - 1)
      .find(combo => expenses.contains(2020 - combo.sum))
      .map(combo => (2020 - combo.sum) :: combo)
      .get
