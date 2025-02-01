package advent2016
import scala.annotation.tailrec

object Day12:
  def part1(input: Vector[String]): Int =
    execute(Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0, "pc" -> 0), input)("a")

  def part2(input: Vector[String]): Int =
    execute(Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0, "pc" -> 0), input)("a")

  @tailrec
  private def execute(registers: Map[String, Int], instructions: Vector[String]): Map[String, Int] =
    if registers("pc") >= instructions.size then
      registers
    else
      instructions(registers("pc")) match
        case s"inc $x"    => execute(registers.updated(x, registers(x) + 1).updated("pc", registers("pc") + 1), instructions)
        case s"dec $x"    => execute(registers.updated(x, registers(x) - 1).updated("pc", registers("pc") + 1), instructions)
        case s"cpy $x $y" => execute(registers.updated(y, registers.getOrElse(x, x.toInt)).updated("pc", registers("pc") + 1), instructions)
        case s"jnz $x $y" =>
          if registers.getOrElse(x, x.toInt) == 0 then
            execute(registers.updated("pc", registers("pc") + 1), instructions)
          else
            execute(registers.updated("pc", registers("pc") + y.toInt), instructions)

