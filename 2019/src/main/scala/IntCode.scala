package year2019

import cats.data.State
import cats.syntax.all.*
import java.util.concurrent.LinkedTransferQueue

class IntCode(memory: Vector[Long]):
  case class Data(pc: Long, relativeBase: Long, memory: Map[Long, Long])
  type IC[A] = State[Data, A]

  val input  = LinkedTransferQueue[Long]
  val output = LinkedTransferQueue[Long]

  def runSync: Map[Long, Long] =
    (runInstruction >> opcode)
      .iterateUntil(_ == 99)
      .runS(Data(0, 0, memory.zipWithIndex.map((data, index) => (index.toLong, data)).toMap.withDefaultValue(0L)))
      .value
      .memory

  val runInstruction = opcode.map(_ % 100).flatMap{
    case 1 => add
    case 2 => multiply
    case 3 => readInput
    case 4 => writeOutput
    case 5 => jumpIfTrue
    case 6 => jumpIfFalse
    case 7 => lessThan
    case 8 => equals
    case 9 => adjustRelativeBase
    case 99 => halt
  }

  val add = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, x + y)
    _ <- incPc(4)
  yield ()

  val multiply = for
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

  val jumpIfTrue = for
    x <- get(1)
    y <- get(2)
    _ <- if x != 0 then setPc(y) else incPc(3)
  yield ()

  val jumpIfFalse = for
    x <- get(1)
    y <- get(2)
    _ <- if x == 0 then setPc(y) else incPc(3)
  yield ()

  val lessThan = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, if x < y then 1 else 0)
    _ <- incPc(4)
  yield ()

  val equals = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, if x == y then 1 else 0)
    _ <- incPc(4)
  yield ()

  val adjustRelativeBase = for
    x <- get(1)
    _ <- State.modify[Data](data => data.copy(relativeBase=data.relativeBase + x))
    _ <- incPc(2)
  yield ()

  val halt: IC[Unit] = State.empty

  def opcode: IC[Long] =
    State.inspect(data => data.memory(data.pc))

  val POSITION  = 0
  val IMMEDIATE = 1
  val RELATIVE  = 2

  def parameterMode(offset: Long): IC[Long] =
    opcode.map(_ / Math.pow(10, offset.toDouble + 1).toLong % 10)

  def get(offset: Long): IC[Long] = parameterMode(offset).flatMap{
    case POSITION  => State.inspect(data => data.memory(data.memory(data.pc + offset)))
    case IMMEDIATE => State.inspect(data => data.memory(data.pc + offset))
    case RELATIVE  => State.inspect(data => data.memory(data.memory(data.pc + offset) + data.relativeBase))
  }

  def set(offset: Long, value: Long): IC[Unit] = parameterMode(offset).flatMap{
    case POSITION => State.modify[Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset), value)))
    case RELATIVE => State.modify[Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset) + data.relativeBase, value)))
  }

  def incPc(offset: Long): IC[Unit] =
    State.modify[Data](data => data.copy(pc=data.pc + offset))

  def setPc(newPc: Long): IC[Unit] =
    State.modify[Data](_.copy(pc=newPc))
