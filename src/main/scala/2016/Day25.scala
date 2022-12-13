package advent2016
import scala.annotation.tailrec

object Day25:
  def part1(input: Vector[String]): Int =
    Iterator.from(0).find(a => execute(Map("a" -> a, "b" -> 0, "c" -> 0, "d" -> 0, "pc" -> 0), input, List.empty)).get

  def part2(input: Vector[String]): Int =
    ???

  @tailrec
  private def execute(registers: Map[String, Int], instructions: Vector[String], loop: List[(Int, Int)]): Boolean =
    instructions(registers("pc")) match
      case s"out $x"    =>
        if loop.map(_._1).contains(registers("a")) then
          val output = loop.map(_._2)
          val alternates = output.sliding(2).forall{case List(x, y) => x != y}
          output.size % 2 == 0 && alternates
        else
          execute(registers.updated("pc", registers("pc") + 1), instructions, (registers("a"), registers("b")) :: loop)
      case s"inc $x"    => execute(registers.updated(x, registers(x) + 1).updated("pc", registers("pc") + 1), instructions, loop)
      case s"dec $x"    => execute(registers.updated(x, registers(x) - 1).updated("pc", registers("pc") + 1), instructions, loop)
      case s"cpy $x $y" => execute(registers.updated(y, registers.getOrElse(x, x.toInt)).updated("pc", registers("pc") + 1), instructions, loop)
      case s"jnz $x $y" =>
        if registers.getOrElse(x, x.toInt) == 0 then
          execute(registers.updated("pc", registers("pc") + 1), instructions, loop)
        else
          execute(registers.updated("pc", registers("pc") + y.toInt), instructions, loop)
