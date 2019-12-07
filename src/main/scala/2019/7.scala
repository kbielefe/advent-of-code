package advent2019
import common.Day
import common.Numeric._
import scala.io.Source
import monix.eval.Task
import cats.effect.concurrent.MVar
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class Intcode(id: String, input: MVar[Task, Int], output: MVar[Task, Int]) {
  type Memory = Vector[Int]

  def run(memory: Memory, pc: Int): Task[Unit] = {
    def mode(param: Int): Int =
      memory(pc).digits.reverse.drop(1).toVector.applyOrElse[Int, Int](param, _ => 0)

    def read(param: Int): Int =
      if (mode(param) == 0) // position
        memory(memory(pc + param))
      else // immediate
        memory(pc + param)

    def write(param: Int, value: Int): Task[Memory] =
      Task{memory.updated(memory(pc + param), value)}

    def opcode(instruction: Int): Int = instruction % 100

    def getParamString(param: Int): String = {
      if (mode(param) == 0)
        s"*${memory(pc + param)}(${memory(memory(pc + param))})"
      else
        s"${memory(pc + param)}"
    }

    def log(op: String, params: Int): Task[Unit] =
      Task{println(s"$id $pc $op ${memory(pc)} ${(1 to params).map(getParamString).mkString(" ")}")}

    def binaryOp(op: String, f: (Int, Int) => Int): Task[Unit] = for {
      _   <- log(op, 3)
      mem <- write(3, f(read(1), read(2)))
      _   <- run(mem, pc + 4)
    } yield ()

    def compareOp(op: String, f: (Int, Int) => Boolean): Task[Unit] =
      binaryOp(op, (a, b) => if (f(a, b)) 1 else 0)

    val add      =  binaryOp("add",      _ + _)
    val multiply =  binaryOp("multiply", _ * _)
    val lessThan = compareOp("lessThan", _ < _)
    val equalTo  = compareOp("equalTo",  _ == _)

    val readInput = for {
      value <- input.take
      _     <- log(s"input($value)", 1)
      mem   <- write(1, value)
      _     <- run(mem, pc + 2)
    } yield ()

    val writeOutput = for {
      _ <- log("output", 1)
      _ <- output.put(read(1))
      _ <- run(memory, pc + 2)
    } yield ()

    val jumpIfTrue = for {
      _ <- log("jumpIfTrue", 2)
      _ <- if (read(1) != 0) run(memory, read(2)) else run(memory, pc + 3)
    } yield ()

    def jumpIfFalse = for {
      _ <- log("jumpIfFalse", 2)
      _ <- if (read(1) == 0) run(memory, read(2)) else run(memory, pc + 3)
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
      case 99 => halt
    }
  }
}

class Day7(source: Source) extends Day {
  val initialMemory = source.getLines.next.split(",").map(_.toInt).toVector

  def runNonFeedback(memory: Vector[Int])(phases: List[Int]): Task[Int] = for {
    inA  <- MVar.of[Task, Int](phases(0))
    aToB <- MVar.of[Task, Int](phases(1))
    bToC <- MVar.of[Task, Int](phases(2))
    cToD <- MVar.of[Task, Int](phases(3))
    dToE <- MVar.of[Task, Int](phases(4))
    outE <- MVar.empty[Task, Int]
    a <- new Intcode("a", inA,  aToB).run(memory, 0).start
    _ <- inA.put(0)
    b <- new Intcode("b", aToB, bToC).run(memory, 0).start
    c <- new Intcode("c", bToC, cToD).run(memory, 0).start
    d <- new Intcode("d", cToD, dToE).run(memory, 0).start
    e <- new Intcode("e", dToE, outE).run(memory, 0).start
    _ <- Task.gatherUnordered(List(a.join, b.join, c.join, d.join, e.join))
    result <- outE.take
  } yield result

  def runWithFeedback(memory: Vector[Int])(phases: List[Int]): Task[Int] = for {
    eToA <- MVar.of[Task, Int](phases(0))
    aToB <- MVar.of[Task, Int](phases(1))
    bToC <- MVar.of[Task, Int](phases(2))
    cToD <- MVar.of[Task, Int](phases(3))
    dToE <- MVar.of[Task, Int](phases(4))
    a <- new Intcode("a", eToA,  aToB).run(memory, 0).start
    _ <- eToA.put(0)
    b <- new Intcode("b", aToB, bToC).run(memory, 0).start
    c <- new Intcode("c", bToC, cToD).run(memory, 0).start
    d <- new Intcode("d", cToD, dToE).run(memory, 0).start
    e <- new Intcode("e", dToE, eToA).run(memory, 0).start
    _ <- Task.gatherUnordered(List(a.join, b.join, c.join, d.join, e.join))
    result <- eToA.take
  } yield result

  override def answer1 = Task.gatherUnordered((0 to 4).toList.permutations.toList.map(runNonFeedback(initialMemory))).map(_.max).runSyncUnsafe(5 seconds).toString

  override def answer2 = Task.gatherUnordered((5 to 9).toList.permutations.toList.map(runWithFeedback(initialMemory))).map(_.max).runSyncUnsafe(5 seconds).toString
}
