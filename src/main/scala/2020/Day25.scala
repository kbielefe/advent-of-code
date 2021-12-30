package advent2020

object Day25:
  def part1(input: List[Long]): Long =
    val loop = loopSize(input(0))
    Iterator.iterate(1L)(transform(input(1))).drop(loop.toInt).next

  private def loopSize(publicKey: Long): Long =
    Iterator.iterate(1L)(transform(7)).indexWhere(_ == publicKey)

  private def transform(subjectNumber: Long)(value: Long): Long =
    subjectNumber * value % 20201227

  def part2(input: List[Long]): String =
    ???
