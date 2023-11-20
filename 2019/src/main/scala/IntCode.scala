package year2019

import cats.data.State
import cats.syntax.all.*
import java.util.concurrent.LinkedTransferQueue

class IntCode(memory: Vector[Int]):
  case class Data(pc: Int, memory: Vector[Int])
  type IC[A] = State[Data, A]

  val input  = LinkedTransferQueue[Int]
  val output = LinkedTransferQueue[Int]

  def runSync: Vector[Int] =
    (runInstruction >> opcode)
      .iterateUntil(_ == 99)
      .runS(Data(0, memory))
      .value
      .memory

  val runInstruction: IC[Unit] = opcode.map(_ % 100).flatMap{
    case 1  => add
    case 2  => multiply
    case 3  => readInput
    case 4  => writeOutput
    case 99 => halt
  }

  val add: IC[Unit] = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, x + y)
    _ <- incPc(4)
  yield ()

  val multiply: IC[Unit] = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, x * y)
    _ <- incPc(4)
  yield ()

  def readInput: IC[Unit] = for
    _ <- set(1, input.take)
    _ <- incPc(2)
  yield ()

  def writeOutput: IC[Unit] = for
    x <- get(1)
    _  = output.put(x)
    _ <- incPc(2)
  yield ()

  val halt: IC[Unit] = State.empty

  def opcode: IC[Int] =
    State.inspect(data => data.memory(data.pc))

  val POSITION  = 0
  val IMMEDIATE = 1

  def parameterMode(offset: Int): IC[Int] =
    opcode.map(_ / Math.pow(10, offset + 1).toInt % 10)

  def get(offset: Int): IC[Int] = parameterMode(offset).flatMap{
    case POSITION  => State.inspect(data => data.memory(data.memory(data.pc + offset)))
    case IMMEDIATE => State.inspect(data => data.memory(data.pc + offset))
  }

  def set(offset: Int, value: Int): IC[Unit] =
    State.modify[Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset), value)))

  def incPc(offset: Int): IC[Unit] =
    State.modify[Data](data => data.copy(pc=data.pc + offset))
