import scala.io.Source
import scala.util.Try
import scala.annotation.tailrec
import scala.collection.immutable.Queue

val input = Source.fromFile("input18.txt").getLines.map(_ split " ").toVector

def getArg(registers: Map[String, Long])(arg: String): Long =
  Try(arg.toLong).getOrElse(registers.getOrElse(arg, 0))

def execute = Iterator.iterate((Map.empty[String, Long], Vector.empty[(String, Long)])){case (regs, actions) =>
  def reg = getArg(regs) _
  val pc = reg("pc").toInt
  val Array(instruction, arg1, arg2) = input(pc).padTo(3, "")
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
    case "snd"                     => actions :+ (("snd", reg(arg1)))
    case "rcv" if (reg(arg1) != 0) => actions :+ (("rcv", reg("lastSound")))
    case _                         => actions
  }
  (newRegs + ("pc" -> newPc), newActions)
}

val answer1 = execute.map(_._2).filterNot(_.isEmpty).dropWhile(_.last._1 == "snd").map(_.last._2).next
println(answer1)

@tailrec
def execute2(regs: Map[String, Long], from: Queue[Long], to: Queue[Long], count: Int): (Map[String, Long], Queue[Long], Int) = {
  def reg = getArg(regs) _
  val pc = reg("pc").toInt

  if (pc < 0 || pc >= input.size || (input(pc)(0) == "rcv" && from.size == 0)) {
    (regs, to, count)
  } else {
    val Array(instruction, arg1, arg2) = input(pc).padTo(3, "")
    val newPc = if (instruction == "jgz" && reg(arg1) > 0) pc + reg(arg2) else pc + 1
    val newRegs = instruction match {
      case "rcv" => regs + (arg1 -> from.front)
      case "set" => regs + (arg1 -> reg(arg2))
      case "add" => regs + (arg1 -> (reg(arg1) + reg(arg2)))
      case "mul" => regs + (arg1 -> reg(arg1) * reg(arg2))
      case "mod" => regs + (arg1 -> reg(arg1) % reg(arg2))
      case _     => regs
    }
    val newFrom = instruction match {
      case "rcv" => from.dequeue._2
      case _     => from
    }
    val newTo = instruction match {
      case "snd" => to.enqueue(reg(arg1))
      case _     => to
    }
    val newCount = if (instruction == "snd") count + 1 else count
    execute2(newRegs + ("pc" -> newPc), newFrom, newTo, newCount)
  }
}

@tailrec
def interleaveExecution(q0: Queue[Long], count0: Int, count1: Int, regs0: Map[String, Long], regs1: Map[String, Long]): Int = {
  val (newRegs0, newq1, newCount0) = execute2(regs0, q0, Queue.empty[Long], count0)
  val (newRegs1, newq0, newCount1) = execute2(regs1, newq1, Queue.empty[Long], count1)
  if (newq1.size == 0 && newq0.size == 0)
    newCount1
  else
    interleaveExecution(newq0, newCount0, newCount1, newRegs0, newRegs1)
}

val answer2 = interleaveExecution(Queue.empty[Long], 0, 0, Map("p" -> 0), Map("p" -> 1))
println(answer2)
