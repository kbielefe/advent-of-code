import scala.io.Source
import scala.util.Try

val input = Source.fromFile("input18.txt").getLines.map(_ split " ").toVector

def getArg(registers: Map[String, Int], arg: String): Int =
  Try(arg.toInt).getOrElse(registers.getOrElse(arg, 0))

def execute = Iterator.iterate((Map.empty[String, Int], Vector.empty[(String, Int)])){case (regs, actions) =>
  val pc = getArg(regs, "pc")
  val Array(instruction, arg1, arg2) = input(pc).padTo(3, "")
  val newPc = if (instruction == "jgz" && getArg(regs, arg1) > 0) pc + getArg(regs, arg2) else pc + 1
  val newRegs = instruction match {
    case "snd" => regs + ("lastSound" -> getArg(regs, arg1))
    case "set" => regs + (arg1 -> getArg(regs, arg2))
    case "add" => regs + (arg1 -> (getArg(regs, arg1) + getArg(regs, arg2)))
    case "mul" => regs + (arg1 -> getArg(regs, arg1) * getArg(regs, arg2))
    case "mod" => regs + (arg1 -> getArg(regs, arg1) % getArg(regs, arg2))
    case _     => regs
  }
  val newActions = instruction match {
    case "snd"                              => actions :+ (("snd", getArg(regs, arg1)))
    case "rcv" if (getArg(regs, arg1) != 0) => actions :+ (("rcv", getArg(regs, "lastSound")))
    case _                                  => actions
  }
  (newRegs + ("pc" -> newPc), newActions)
}

val answer1 = execute.map(_._2).dropWhile(_.dropWhile(_._1 == "snd").isEmpty).map(_.last._2).next
println(answer1)
