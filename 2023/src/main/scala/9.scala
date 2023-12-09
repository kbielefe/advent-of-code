package day9
import parse.{*, given}

type I = List[List[Long] - " "] - "\n"

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    input.map(extrapolate).sum

  def part2(input: I): Long =
    input.map(_.reverse).map(extrapolate).sum

  def extrapolate(seq: List[Long]): Long =
    if seq.forall(_ == 0) then
      0
    else
      val diffs = seq.sliding(2).collect{case List(x, y) => y - x}
      seq.last + extrapolate(diffs.toList)
