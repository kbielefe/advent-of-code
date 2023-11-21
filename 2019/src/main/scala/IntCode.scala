package year2019

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.std.*

object IntCode:
  def apply(memory: Vector[Long]): IO[IntCode] = for
    input  <- Queue.unbounded[IO, Long]
    output <- Queue.unbounded[IO, Long]
    memMap  = memory.zipWithIndex.map((data, index) => (index.toLong, data)).toMap.withDefaultValue(0L)
  yield new IntCode(input, output, memMap)

class IntCode private (inputQueue: Queue[IO, Long], outputQueue: Queue[IO, Long], memory: Map[Long, Long]):
  case class Data(pc: Long, relativeBase: Long, memory: Map[Long, Long])
  type IC[A] = StateT[IO, Data, A]

  def input(x: Long): IO[Unit] = inputQueue.offer(x)
  def output: IO[Long] = outputQueue.take

  def run: IO[Map[Long, Long]] =
    (runInstruction >> opcode)
      .iterateUntil(_ == 99)
      .runS(Data(0, 0, memory))
      .map(_.memory)

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
   input <- StateT[IO, Data, Long](data => inputQueue.take.map(data -> _))
    _    <- set(1, input)
    _    <- incPc(2)
  yield ()

  def writeOutput: IC[Unit] = for
    x <- get(1)
    _ <- StateT[IO, Data, Unit](data => outputQueue.offer(x).map(data -> _))
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
    _ <- StateT.modify[IO, Data](data => data.copy(relativeBase=data.relativeBase + x))
    _ <- incPc(2)
  yield ()

  val halt: IC[Unit] = StateT.empty

  def opcode: IC[Long] =
    StateT.inspect(data => data.memory(data.pc))

  val POSITION  = 0
  val IMMEDIATE = 1
  val RELATIVE  = 2

  def parameterMode(offset: Long): IC[Long] =
    opcode.map(_ / Math.pow(10, offset.toDouble + 1).toLong % 10)

  def get(offset: Long): IC[Long] = parameterMode(offset).flatMap{
    case POSITION  => StateT.inspect(data => data.memory(data.memory(data.pc + offset)))
    case IMMEDIATE => StateT.inspect(data => data.memory(data.pc + offset))
    case RELATIVE  => StateT.inspect(data => data.memory(data.memory(data.pc + offset) + data.relativeBase))
  }

  def set(offset: Long, value: Long): IC[Unit] = parameterMode(offset).flatMap{
    case POSITION => StateT.modify[IO, Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset), value)))
    case RELATIVE => StateT.modify[IO, Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset) + data.relativeBase, value)))
  }

  def incPc(offset: Long): IC[Unit] =
    StateT.modify[IO, Data](data => data.copy(pc=data.pc + offset))

  def setPc(newPc: Long): IC[Unit] =
    StateT.modify[IO, Data](_.copy(pc=newPc))
