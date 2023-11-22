package year2019

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.std.*
import java.io.PrintStream

class IntCodeException(stack: List[String]) extends Exception("Error executing IntCode"):
  override def printStackTrace(ps: PrintStream): Unit =
    val trace = s"Error executing IntCode:\n  ${stack.reverse.mkString("\n  ")}"
    ps.print(trace)

object IntCode:
  def apply(memory: Vector[Long], trace: Boolean = false, id: Int = 0, onInputBlock: Option[Int => IO[Long]] = None): IO[IntCode] = for
    input  <- Queue.unbounded[IO, Long]
    output <- Queue.unbounded[IO, Long]
    mutex  <- Mutex[IO]
    memMap  = memory.zipWithIndex.map((data, index) => (index.toLong, data)).toMap.withDefaultValue(0L)
  yield new IntCode(input, output, memMap, mutex, trace, id, onInputBlock)

class IntCode private (
  inputQueue: Queue[IO, Long],
  outputQueue: Queue[IO, Long],
  memory: Map[Long, Long],
  inputMutex: Mutex[IO],
  trace: Boolean,
  id: Int,
  onInputBlock: Option[Int => IO[Long]]):
  case class Data(pc: Long, stack: List[String], relativeBase: Long, memory: Map[Long, Long])
  type IC[A] = StateT[IO, Data, A]

  def input(xs: Long*): IO[Unit] =
    inputMutex.lock.surround(xs.iterator.to(List).traverse(inputQueue.offer).void)

  def output: IO[Long] = outputQueue.take

  def run: IO[Map[Long, Long]] =
    (runInstruction >> opcode)
      .iterateUntil(_ == 99)
      .runS(Data(0, List.empty, 0, memory))
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
    case _  => error
  }

  val add = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, x + y)
    _ <- incPc(4)
  yield ()

  val addString = for
    x <- getString(1)
    y <- getString(2)
    r <- setString(3)
  yield s"add $x $y $r"

  val multiply = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, x * y)
    _ <- incPc(4)
  yield ()

  val multiplyString = for
    x <- getString(1)
    y <- getString(2)
    r <- setString(3)
  yield s"multiply $x $y $r"

  def readInput: IC[Unit] = for
   input <- readInputQueue
    _    <- set(1, input)
    _    <- incPc(2)
  yield ()

  val readInputString = for
    x <- setString(1)
  yield s"input $x"

  def writeOutput: IC[Unit] = for
    x <- get(1)
    _ <- writeOutputQueue(x)
    _ <- incPc(2)
  yield ()

  val writeOutputString = for
    x <- getString(1)
  yield s"output $x"

  val jumpIfTrue = for
    x <- get(1)
    y <- get(2)
    _ <- if x != 0 then setPc(y) else incPc(3)
  yield ()

  val jumpIfTrueString = for
    x <- getString(1)
    y <- getString(2)
  yield s"jumpIfTrue $x $y"

  val jumpIfFalse = for
    x <- get(1)
    y <- get(2)
    _ <- if x == 0 then setPc(y) else incPc(3)
  yield ()

  val jumpIfFalseString = for
    x <- getString(1)
    y <- getString(2)
  yield s"jumpIfFalse $x $y"

  val lessThan = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, if x < y then 1 else 0)
    _ <- incPc(4)
  yield ()

  val lessThanString = for
    x <- getString(1)
    y <- getString(2)
    z <- setString(3)
  yield s"lessThan $x $y $z"

  val equals = for
    x <- get(1)
    y <- get(2)
    _ <- set(3, if x == y then 1 else 0)
    _ <- incPc(4)
  yield ()

  val equalsString = for
    x <- getString(1)
    y <- getString(2)
    z <- setString(3)
  yield s"equals $x $y"

  val adjustRelativeBase = for
    x <- get(1)
    _ <- StateT.modify[IO, Data](data => data.copy(relativeBase=data.relativeBase + x))
    _ <- incPc(2)
  yield ()

  val adjustRelativeBaseString = for
    x <- getString(1)
  yield s"adjustRelativeBase $x"

  val halt: IC[Unit] = StateT.empty

  val haltString = StateT.pure[IO, Data, String]("halt")

  val errorString = for
    x <- opcode
  yield s"invalid opcode $x"

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

  def getString(offset: Long): IC[String] = parameterMode(offset).flatMap{
    case POSITION  => StateT.inspect(data => s"p[${data.pc}, $offset]<-${data.memory(data.memory(data.pc + offset))}")
    case IMMEDIATE => StateT.inspect(data => s"i[${data.pc}, $offset]<-${data.memory(data.pc + offset)}")
    case RELATIVE  => StateT.inspect(data => s"r[${data.pc}, $offset, ${data.relativeBase}]<-${data.memory(data.memory(data.pc + offset) + data.relativeBase)}")
  }

  def set(offset: Long, value: Long): IC[Unit] = parameterMode(offset).flatMap{
    case POSITION => StateT.modify[IO, Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset), value)))
    case RELATIVE => StateT.modify[IO, Data](data => data.copy(memory=data.memory.updated(data.memory(data.pc + offset) + data.relativeBase, value)))
  }

  def setString(offset: Long): IC[String] = parameterMode(offset).flatMap{
    case POSITION  => StateT.inspect(data => s"p[${data.pc}, $offset]->${data.memory(data.memory(data.pc + offset))}")
    case RELATIVE  => StateT.inspect(data => s"r[${data.pc}, $offset, ${data.relativeBase}]->${data.memory(data.memory(data.pc + offset) + data.relativeBase)}")
  }

  def incPc(offset: Long): IC[Unit] =
    StateT.inspect[IO, Data, Long](_.pc).flatMap(pc => setPc(pc + offset))

  def setPc(newPc: Long): IC[Unit] =
    pushStack >>
    StateT.modify[IO, Data](_.copy(pc=newPc))

  val pushStack: IC[Unit] =
    if trace then generateString.flatMap(pushString) else StateT.empty

  val generateString: IC[String] = opcode.map(_ % 100).flatMap{
    case 1 => addString
    case 2 => multiplyString
    case 3 => readInputString
    case 4 => writeOutputString
    case 5 => jumpIfTrueString
    case 6 => jumpIfFalseString
    case 7 => lessThanString
    case 8 => equalsString
    case 9 => adjustRelativeBaseString
    case 99 => haltString
    case _  => errorString
  }

  def pushString(string: String): IC[Unit] =
    StateT.modify[IO, Data](data => data.copy(stack=s"${data.pc}: $string"::data.stack))

  val readInputQueue: IC[Long] = StateT.liftF(
    onInputBlock match
      case Some(f) => inputQueue.tryTake.flatMap {
        case Some(input) => IO.pure(input)
        case None        => f(id)
      }
      case None => inputQueue.take
  )

  def writeOutputQueue(x: Long): IC[Unit] =
    StateT.liftF(outputQueue.offer(x))

  def error: IC[Unit] = for
    _     <- pushStack
    stack <- StateT.inspect[IO, Data, List[String]](_.stack)
    _     <- StateT.liftF(IO.raiseError(IntCodeException(stack)))
  yield ()
