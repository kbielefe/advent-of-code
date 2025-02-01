package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day8 extends ConsoleDay[Int, Int](2020, 8) {
  @scala.annotation.tailrec
  private def findRepeatAcc(instructions: Vector[ConsoleInstruction], hasExecuted: Vector[Boolean], acc: Int, pc: Int): Int = {
    if (hasExecuted(pc))
      acc
    else
      instructions(pc) match {
        case Nop(offset) => findRepeatAcc(instructions, hasExecuted.updated(pc, true), acc, pc + 1)
        case Jmp(offset) => findRepeatAcc(instructions, hasExecuted.updated(pc, true), acc, pc + offset)
        case Acc(offset) => findRepeatAcc(instructions, hasExecuted.updated(pc, true), acc + offset, pc + 1)
      }
  }

  @scala.annotation.tailrec
  private def findTerminateAcc(instructions: Vector[ConsoleInstruction], hasExecuted: Vector[Boolean], acc: Int, pc: Int): Option[Int] = {
    if (pc >= instructions.size)
      Some(acc)
    else if (hasExecuted(pc))
      None
    else
      instructions(pc) match {
        case Nop(offset) => findTerminateAcc(instructions, hasExecuted.updated(pc, true), acc, pc + 1)
        case Jmp(offset) => findTerminateAcc(instructions, hasExecuted.updated(pc, true), acc, pc + offset)
        case Acc(offset) => findTerminateAcc(instructions, hasExecuted.updated(pc, true), acc + offset, pc + 1)
      }
  }

  private def fixCorruption(instructions: Vector[ConsoleInstruction]): Int =
    (0 until instructions.size).map{replaceIndex =>
      val fixedInstructions = instructions(replaceIndex) match {
        case Nop(offset) => instructions.updated(replaceIndex, Jmp(offset))
        case Jmp(offset) => instructions.updated(replaceIndex, Nop(offset))
        case Acc(offset) => instructions
      }
      findTerminateAcc(fixedInstructions, Vector.fill(instructions.size)(false), 0, 0)
    }.dropWhile(_.isEmpty)
    .head
    .get

  override def part1(input: Vector[ConsoleInstruction]): Int =
    findRepeatAcc(input, Vector.fill(input.size)(false), 0, 0)

  override def part2(input: Vector[ConsoleInstruction]): Int =
    fixCorruption(input)
}
