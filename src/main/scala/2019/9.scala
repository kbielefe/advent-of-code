package advent2019
import common.Day
import common.Numeric._
import scala.io.Source
import monix.eval.Task
import cats.effect.concurrent.MVar
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class Intcode(id: String, input: MVar[Task, Long], output: MVar[Task, Long]) {
  type Memory = Vector[Long]

  def run(memory: Memory, pc: Int, relativeBase: Int): Task[Unit] = {
    def mode(param: Long): Int =
      memory(pc).digits.reverse.drop(1).toVector.applyOrElse[Int, Int](param.toInt, _ => 0)

    def read(param: Int): Long = mode(param) match {
      case 0 /* position */  => memory.applyOrElse(memory.applyOrElse(pc + param, _ => 0).toInt, _ => 0)
      case 1 /* immediate */ => memory.applyOrElse(pc + param, _ => 0)
      case 2 /* relative */  => memory.applyOrElse(relativeBase + param, _ => 0)
    }

    def write(param: Int, value: Long): Task[Memory] =
      Task{memory.updated(memory.applyOrElse(pc + param, _ => 0).toInt, value)}

    def opcode(instruction: Long): Long = instruction % 100

    def getParamString(param: Int): String = {
      if (mode(param) == 0)
        s"*${memory.applyOrElse(pc + param, _ => 0)}(${memory.applyOrElse(memory.applyOrElse(pc + param, _ => 0).toInt, _ => 0)})"
      else
        s"${memory.applyOrElse(pc + param, _ => 0)}"
    }

    def log(op: String, params: Int): Task[Unit] =
      Task{println(s"$id $pc $op ${memory(pc)} ${(1 to params).map(getParamString).mkString(" ")}")}

    def binaryOp(op: String, f: (Long, Long) => Long): Task[Unit] = for {
      _   <- log(op, 3)
      mem <- write(3, f(read(1), read(2)))
      _   <- run(mem, pc + 4, relativeBase)
    } yield ()

    def compareOp(op: String, f: (Long, Long) => Boolean): Task[Unit] =
      binaryOp(op, (a, b) => if (f(a, b)) 1 else 0)

    val add      =  binaryOp("add",      _ + _)
    val multiply =  binaryOp("multiply", _ * _)
    val lessThan = compareOp("lessThan", _ < _)
    val equalTo  = compareOp("equalTo",  _ == _)

    val readInput = for {
      value <- input.take
      _     <- log(s"input($value)", 1)
      mem   <- write(1, value)
      _     <- run(mem, pc + 2, relativeBase)
    } yield ()

    val writeOutput = for {
      _ <- log("output", 1)
      _ <- output.put(read(1))
      _ <- run(memory, pc + 2, relativeBase)
    } yield ()

    val jumpIfTrue = for {
      _ <- log("jumpIfTrue", 2)
      _ <- if (read(1) != 0) run(memory, read(2).toInt, relativeBase) else run(memory, pc + 3, relativeBase)
    } yield ()

    def jumpIfFalse = for {
      _ <- log("jumpIfFalse", 2)
      _ <- if (read(1) == 0) run(memory, read(2).toInt, relativeBase) else run(memory, pc + 3, relativeBase)
    } yield ()

    val adjustRelativeBase = for {
      _   <- log("adjustRelativeBase", 1)
      _   <- run(memory, pc + 2, relativeBase + read(1).toInt)
    } yield ()

    val halt = log("halt", 0)

    opcode(memory(pc)) match {
      case  1 => add
      case  2 => multiply
      case  3 => readInput
      case  4 => writeOutput
      case  5 => jumpIfTrue
      case  6 => jumpIfFalse
      case  7 => lessThan
      case  8 => equalTo
      case  9 => adjustRelativeBase
      case 99 => halt
    }
  }
}

class Day9(source: Source) extends Day {
  val initialMemory = source.getLines.next.split(",").map(_.toLong).toVector

  val runBoost = for {
    in     <- MVar.of[Task, Long](1)
    out    <- MVar.empty[Task, Long]
    boost  <- new Intcode("boost", in, out).run(initialMemory, 0, 0)
    result <- out.take
  } yield result

  override def answer1 = runBoost.runSyncUnsafe(5 seconds).toString

  override def answer2 = "unimplemented"
}
