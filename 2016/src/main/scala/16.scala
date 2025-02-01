package advent2016
import scala.annotation.tailrec

object Day16:
  def part1(input: String): String =
    checksum(dragonCurve(input.trim, 272))

  def part2(input: String): String =
    checksum(dragonCurve(input.trim, 35651584))

  @tailrec
  def dragonCurve(state: String, take: Int): String =
    if state.length >= take then
      state.take(take)
    else
      dragonCurve(state + "0" + state.reverse.map(x => if x == '1' then '0' else '1'), take)

  @tailrec
  def checksum(state: String): String =
    if state.length % 2 == 1 then
      state
    else
      checksum(state.grouped(2).map(x => if x(0) == x(1) then "1" else "0").mkString)
