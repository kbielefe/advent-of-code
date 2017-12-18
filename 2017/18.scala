import scala.io.Source
import scala.util.Try

val input = Source.fromFile("input18.txt").getLines.map(_ split " ").toVector

def getArg(registers: Map[String, Long])(arg: String): Long =
  Try(arg.toLong).getOrElse(registers.getOrElse(arg, 0))

def execute = Iterator.iterate((Map.empty[String, Long], Vector.empty[(String, Long)])){case (regs, actions) =>
  def reg = getArg(regs) _
  val pc = reg("pc")
  val Array(instruction, arg1, arg2) = input(pc.toInt).padTo(3, "")
  val newPc = if (instruction == "jgz" && reg(arg1) > 0) pc + reg(arg2) else pc + 1
  val newRegs = instruction match {
    case "snd" => regs + ("lastSound" -> reg(arg1))
    case "set" => regs + (arg1 -> reg(arg2))
    case "add" => regs + (arg1 -> (reg(arg1) + reg(arg2)))
    case "mul" => regs + (arg1 -> reg(arg1) * reg(arg2))
    case "mod" => regs + (arg1 -> reg(arg1) % reg(arg2))
    case _     => regs
  }
  val newActions = instruction match {
    case "snd"                              => actions :+ (("snd", reg(arg1)))
    case "rcv" if (reg(arg1) != 0) => actions :+ (("rcv", reg("lastSound")))
    case _                                  => actions
  }
  (newRegs + ("pc" -> newPc), newActions)
}

val answer1 = execute.map(_._2).filterNot(_.isEmpty).dropWhile(_.last._1 == "snd").map(_.last._2).next
println(answer1)
