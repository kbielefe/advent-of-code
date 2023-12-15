package day10
import parse.{*, given}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Puzzle extends runner.Day[String, Int, Int]:
  def part1(input: String): Int =
    val buffer = ArrayBuffer.from(input.map(_.asDigit.toByte))
    lookAndSay(0, 0, 0, buffer, ArrayBuffer.empty, 40).size

  def part2(input: String): Int =
    val buffer = ArrayBuffer.from(input.map(_.asDigit.toByte))
    lookAndSay(0, 0, 0, buffer, ArrayBuffer.empty, 50).size

  @tailrec
  def lookAndSay(index: Int, digit: Byte, count: Byte, in: ArrayBuffer[Byte], out: ArrayBuffer[Byte], row: Int): ArrayBuffer[Byte] =
    if row == 0 then
      in
    else if index == 0 then
      out.clear()
      lookAndSay(1, in(0), 1, in, out, row)
    else if index == in.size then
      lookAndSay(0, 0, 0, out += count += digit, in, row - 1)
    else if in(index) == digit then
      lookAndSay(index + 1, digit, (count + 1).toByte, in, out, row)
    else
      lookAndSay(index + 1, in(index), 1, in, out += count += digit, row)

