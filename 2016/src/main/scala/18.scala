package advent2016
import scala.annotation.tailrec

object Day18:
  def part1(input: String): Int =
    countSafe(input.trim, 40, 0)

  def part2(input: String): Int =
    countSafe(input.trim, 400000, 0)

  @tailrec
  private def countSafe(above: String, row: Int, count: Int): Int =
    if row == 0 then
      count
    else
      val safeAbove = above.count(_ == '.')
      val next = ("." + above + ".").sliding(3).map{
        case "^^." => '^'
        case ".^^" => '^'
        case "^.." => '^'
        case "..^" => '^'
        case _     => '.'
      }.mkString
      countSafe(next, row - 1, count + safeAbove)
