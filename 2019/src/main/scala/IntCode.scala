package year2019

import cats.data.State
import cats.syntax.all.*

class IntCode(input: Vector[Int]):
  case class Data(pc: Int, memory: Vector[Int])
  type IC[A] = State[Data, A]

  def runSync: Vector[Int] =
    (runInstruction >> opcode)
      .iterateUntil(_ == 99)
      .runS(Data(0, input))
      .value
      .memory

  val runInstruction: IC[Unit] = opcode.flatMap{
    case 1  => add
    case 2  => multiply
    case 99 => halt
  }

  val add: IC[Unit] = for
    x <- getPtr(1)
    y <- getPtr(2)
    _ <- setPtr(3, x + y)
    _ <- incPc(4)
  yield ()

  val multiply: IC[Unit] = for
    x <- getPtr(1)
    y <- getPtr(2)
    _ <- setPtr(3, x * y)
    _ <- incPc(4)
  yield ()

  val halt: IC[Unit] = State.empty

  def opcode: IC[Int] =
    State.inspect(data => data.memory(data.pc))

  // Get memory from a pointer with an offset relative to the PC
  def getPtr(offset: Int): IC[Int] = State.inspect{data =>
    data.memory(data.memory(data.pc + offset))
  }

  // Set memory from a pointer with an offset relative to the PC
  def setPtr(offset: Int, value: Int): IC[Unit] = State.modify[Data]{data =>
    data.copy(memory=data.memory.updated(data.memory(data.pc + offset), value))
  }

  def incPc(offset: Int): IC[Unit] =
    State.modify[Data](data => data.copy(pc=data.pc + offset))
