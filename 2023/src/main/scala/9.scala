package day9
import parse.{*, given}

type I = List[List[Long] - " "] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    input.map(extrapolate(false)).sum

  def part2(input: I): Long =
    input.map(extrapolate(true)).sum

  def extrapolate(backwards: Boolean)(seq: List[Long]): Long =
    if seq.forall(_ == 0) then
      0
    else
      val diffs = seq.sliding(2).collect{case List(x, y) => y - x}
      if backwards then
        seq.head - extrapolate(true)(diffs.toList)
      else
        seq.last + extrapolate(false)(diffs.toList)
