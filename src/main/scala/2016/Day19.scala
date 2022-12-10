package advent2016
import scala.annotation.tailrec

object Day19:
  def part1(input: Int): Int =
    whiteElephant(Nil, List.fill(input + 1)(1).zipWithIndex.drop(1))

  def part2(input: Int): Int =
    ???

  @tailrec
  private def whiteElephant(behind: List[(Int, Int)], ahead: List[(Int, Int)]): Int =
    if behind.isEmpty && ahead.tail.isEmpty then
      ahead.head._2
    else if ahead.isEmpty then
      whiteElephant(Nil, behind.reverse)
    else if ahead.tail.isEmpty then
      whiteElephant(Nil, ahead.head :: behind.reverse)
    else if ahead.head._1 == 0 then
      whiteElephant(behind, ahead.tail)
    else
      val (head :: next :: tail) = ahead: @unchecked
      whiteElephant(((head._1 + next._1), head._2) :: behind, tail)
