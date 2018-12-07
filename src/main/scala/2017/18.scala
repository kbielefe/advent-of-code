package advent2017
import common.Day
import scala.io.Source
import scala.util.Try
import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Day18(source: Source) extends Day {
  val input = source.getLines.map(_ split " ").toVector

  class Assembly[S](
    instructions: Vector[Array[String]],
    regUpdate:    Map[String, (Long, Long) => Long],
    pcUpdate:     Map[String, (Long, Long) => Long],
    stateUpdate:  Map[String, (Long, Long, S) => S]) {

      def getArg(registers: Map[String, Long])(arg: String): Long =
        Try(arg.toLong).getOrElse(registers.getOrElse(arg, 0))

      def execute(initialState: S) =
        Iterator.iterate((Map.empty[String, Long], 0, initialState)){case (regs, pc, state) =>
          def reg = getArg(regs) _
          val Array(instruction, arg1, arg2) = instructions(pc).padTo(3, "")
          def incPc(x: Long, y: Long): Long = 1
          val newPc = pc + pcUpdate.getOrElse(instruction, incPc _)(reg(arg1), reg(arg2)).toInt
          val newRegs = if (regUpdate contains instruction)
          regs + (arg1 -> regUpdate(instruction)(reg(arg1), reg(arg2)))
        else
          regs
        def keepState(x: Long, y: Long, s: S): S = s
        val newState = stateUpdate.getOrElse(instruction, keepState _)(reg(arg1), reg(arg2), state)
        (newRegs, newPc, newState)
        }
    }

  val regUpdate = Map[String, (Long, Long) => Long](
    "set" -> {case (_, arg2) => arg2},
    "add" -> {_ + _},
    "mul" -> {_ * _},
    "mod" -> {_ % _}
  )
  val pcUpdate = Map[String, (Long, Long) => Long](
    "jgz" -> {case (arg1, arg2) => if (arg1 > 0) arg2 else 1}
  )
  val stateUpdate = Map[String, (Long, Long, Long) => Long](
    "snd" -> {case (arg1, _, _) => arg1}
  )
  val assembly = new Assembly(input, regUpdate, pcUpdate, stateUpdate)

  override def answer1 = assembly.execute(-1).dropWhile{x => input(x._2)(0) != "rcv"}.map(_._3).next.toString

  def getArg(registers: Map[String, Long])(arg: String): Long =
    Try(arg.toLong).getOrElse(registers.getOrElse(arg, 0))

  @tailrec
  private def execute2(regs: Map[String, Long], from: Queue[Long], to: Queue[Long], count: Int): (Map[String, Long], Queue[Long], Int) = {
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
  private def interleaveExecution(q0: Queue[Long], count0: Int, count1: Int, regs0: Map[String, Long], regs1: Map[String, Long]): Int = {
    val (newRegs0, newq1, newCount0) = execute2(regs0, q0, Queue.empty[Long], count0)
    val (newRegs1, newq0, newCount1) = execute2(regs1, newq1, Queue.empty[Long], count1)
    if (newq1.size == 0 && newq0.size == 0)
      newCount1
    else
      interleaveExecution(newq0, newCount0, newCount1, newRegs0, newRegs1)
  }

  override def answer2 = interleaveExecution(Queue.empty[Long], 0, 0, Map("p" -> 0), Map("p" -> 1)).toString
}
