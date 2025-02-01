package advent2016
import scala.annotation.tailrec

object Day23:
  def part1(input: Vector[String]): Int =
    execute(Map("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0, "pc" -> 0), input)("a")

  def part2(input: Vector[String]): Int =
    execute(Map("a" -> 12, "b" -> 0, "c" -> 0, "d" -> 0, "pc" -> 0), input)("a")

  extension (s: String)
    def isRegister: Boolean =
      s == "a" || s == "b" || s == "c" || s == "d"

  @tailrec
  private def execute(registers: Map[String, Int], instructions: Vector[String]): Map[String, Int] =
    if registers("pc") >= instructions.size then
      registers
    else
      instructions(registers("pc")) match
        case s"inc $x" =>
          if x.isRegister then
            execute(registers.updated(x, registers(x) + 1).updated("pc", registers("pc") + 1), instructions)
          else
            execute(registers.updated("pc", registers("pc") + 1), instructions)
        case s"dec $x" =>
          if x.isRegister then
            execute(registers.updated(x, registers(x) - 1).updated("pc", registers("pc") + 1), instructions)
          else
            execute(registers.updated("pc", registers("pc") + 1), instructions)
        case s"cpy $x $y" =>
          if y.isRegister then
            execute(registers.updated(y, registers.getOrElse(x, x.toInt)).updated("pc", registers("pc") + 1), instructions)
          else
            execute(registers.updated("pc", registers("pc") + 1), instructions)
        case s"jnz $x $y" =>
          if registers.getOrElse(x, x.toInt) == 0 then
            execute(registers.updated("pc", registers("pc") + 1), instructions)
          else
            execute(registers.updated("pc", registers("pc") + registers.getOrElse(y, y.toInt)), instructions)
        case s"tgl $x" =>
          val pcToToggle = registers("pc") + registers.getOrElse(x, x.toInt)
          if pcToToggle < 0 || pcToToggle >= instructions.size then
            execute(registers.updated("pc", registers("pc") + 1), instructions)
          else
            val toggled = instructions(pcToToggle) match
              case s"inc $x"    => s"dec $x"
              case s"dec $x"    => s"inc $x"
              case s"tgl $x"    => s"inc $x"
              case s"cpy $x $y" => s"jnz $x $y"
              case s"jnz $x $y" => s"cpy $x $y"
            val newInstructions = instructions.updated(pcToToggle, toggled)
            execute(registers.updated("pc", registers("pc") + 1), newInstructions)
