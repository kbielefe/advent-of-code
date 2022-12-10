package advent2016

object Day16:
  def part1(input: String): String =
    println(dragonCurve("111100001010".iterator).take(20).mkString)
    ???

  def part2(input: String): String =
    ???

  def dragonCurve(state: Iterator[Char]): Iterator[Char] =
    Iterator.iterate(state){state =>
      val (a, b) = state.duplicate
      a ++ "0".iterator ++ b.toSeq.reverseIterator.map(x => if x == '1' then '0' else '1')
    }.flatten
