package advent2016
import scala.annotation.tailrec

object Day21:
  def part1(input: List[String]): String =
    scramble(input, "abcdefgh")

  def part2(input: List[String]): String =
    "abcdefgh".permutations.find(scramble(input, _) == "fbgdceah").get

  @tailrec
  private def scramble(instructions: List[String], input: String): String =
    if instructions.isEmpty then
      input
    else
      val output = instructions.head match
        case s"swap position $x with position $y" =>
          input.updated(x.toInt, input(y.toInt)).updated(y.toInt, input(x.toInt))
        case s"swap letter $x with letter $y" =>
          input.updated(input.indexWhere(_ == x.head), y.head).updated(input.indexWhere(_ == y.head), x.head)
        case s"rotate right $x step$s" =>
          val size = input.size - x.toInt
          input.drop(size) + input.take(size)
        case s"rotate left $x step$s" =>
          input.drop(x.toInt) + input.take(x.toInt)
        case s"rotate based on position of letter $x" =>
          val index = input.indexWhere(_ == x.head)
          val size = input.size - (1 + index + (if index >= 4 then 1 else 0)) % input.size
          input.drop(size) + input.take(size)
        case s"reverse positions $x through $y" =>
          val slice = input.slice(x.toInt, y.toInt + 1)
          input.patch(x.toInt, slice.reverse, y.toInt - x.toInt + 1)
        case s"move position $x to position $y" =>
          input.patch(x.toInt, "", 1).patch(y.toInt, input(x.toInt).toString, 0)

      scramble(instructions.tail, output)
