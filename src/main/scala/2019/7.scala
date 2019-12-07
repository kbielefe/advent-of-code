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

  def read(memory: Memory, pc: Int, param: Int): Task[Int] = Task{
    val mode = memory(pc).digits.reverse.drop(1).toVector.applyOrElse[Int, Int](param, _ => 0)
    if (mode == 0) // position
      memory(memory(pc + param))
    else // immediate
      memory(pc + param)
  }

  def write(memory: Memory, pc: Int, param: Int)(value: Int): Task[Memory] = Task{memory.updated(memory(pc + param), value)}

  def opcode(instruction: Int): Int = instruction % 100

  def run(memory: Memory, pc: Int): Task[Unit] = {
    def getParamString(param: Int): String = {
      val mode = memory(pc).digits.reverse.drop(1).toVector.applyOrElse[Int, Int](param, _ => 0)
      if (mode == 0)
        s"*${memory(pc + param)}(${memory(memory(pc + param))})"
      else
        s"${memory(pc + param)}"
    }

    def log(op: String, params: Int): Task[Unit] = Task{println(s"$id $pc $op ${memory(pc)} ${(1 to params).map(getParamString).mkString(" ")}")}

    opcode(memory(pc)) match {
      case 99 => log("halt", 0)
      case  1 => log("add", 3) flatMap {_ => add(memory, pc)} flatMap {newMemory => run(newMemory, pc + 4)}
      case  2 => log("multiply", 3) flatMap {_ => multiply(memory, pc)} flatMap {newMemory => run(newMemory, pc + 4)}
      case  3 => input.take flatMap {value => Task{println(s"$id read $value"); value}} flatMap write(memory, pc, 1) flatMap {newMemory => run(newMemory, pc + 2)}
      case  4 => log("output", 1) flatMap {_ => read(memory, pc, 1)} flatMap output.put flatMap {_ => run(memory, pc + 2)}
      case  5 => log("jumpIfTrue", 2) flatMap {_ => jumpIfTrue(memory, pc)}
      case  6 => log("jumpIfFalse", 2) flatMap {_ => jumpIfFalse(memory, pc)}
      case  7 => log("lessThan", 3) flatMap {_ => lessThan(memory, pc)} flatMap {newMemory => run(newMemory, pc + 4)}
      case  8 => log("equalTo", 3) flatMap {_ => equalTo(memory, pc)} flatMap {newMemory => run(newMemory, pc + 4)}
    }
  }

  def add(memory: Memory, pc: Int): Task[Memory] = for {
    lhs <- read(memory, pc, 1)
    rhs <- read(memory, pc, 2)
    mem   <- write(memory, pc, 3)(lhs + rhs)
  } yield mem

  def multiply(memory: Memory, pc: Int): Task[Memory] = for {
    lhs <- read(memory, pc, 1)
    rhs <- read(memory, pc, 2)
    mem <- write(memory, pc, 3)(lhs * rhs)
  } yield mem

  def jumpIfTrue(memory: Memory, pc: Int): Task[Unit] = for {
    cond   <- read(memory, pc, 1)
    target <- read(memory, pc, 2)
    _ <- if (cond != 0) run(memory, target) else run(memory, pc + 3)
  } yield ()

  def jumpIfFalse(memory: Memory, pc: Int): Task[Unit] = for {
    cond   <- read(memory, pc, 1)
    target <- read(memory, pc, 2)
    _ <- if (cond == 0) run(memory, target) else run(memory, pc + 3)
  } yield ()

  def lessThan(memory: Memory, pc: Int): Task[Memory] = for {
    lhs <- read(memory, pc, 1)
    rhs <- read(memory, pc, 2)
    mem <- write(memory, pc, 3)(if (lhs < rhs) 1 else 0)
  } yield mem

  def equalTo(memory: Memory, pc: Int): Task[Memory] = for {
    lhs <- read(memory, pc, 1)
    rhs <- read(memory, pc, 2)
    mem <- write(memory, pc, 3)(if (lhs == rhs) 1 else 0)
  } yield mem
}

class Day7(source: Source) extends Day {
  val initialMemory = source.getLines.next.split(",").map(_.toInt).toVector

  def runNonFeedback(memory: Vector[Int])(phases: List[Int]): Task[Int] = for {
    _ <- Task{println(phases)}
    inA  <- MVar.of[Task, Int](phases(0))
    aToB <- MVar.of[Task, Int](phases(1))
    bToC <- MVar.of[Task, Int](phases(2))
    cToD <- MVar.of[Task, Int](phases(3))
    dToE <- MVar.of[Task, Int](phases(4))
    outE <- MVar.empty[Task, Int]
    a <- new Intcode("a", inA,  aToB).run(memory, 0).start
    b <- new Intcode("b", aToB, bToC).run(memory, 0).start
    c <- new Intcode("c", bToC, cToD).run(memory, 0).start
    d <- new Intcode("d", cToD, dToE).run(memory, 0).start
    e <- new Intcode("e", dToE, outE).run(memory, 0).start
    _ <- inA.put(0)
    _ <- Task.gatherUnordered(List(a.join, b.join, c.join, d.join, e.join))
    result <- outE.take
  } yield result

  // 18812
  override def answer1 = Task.gatherUnordered((0 to 4).toList.permutations.toList.map(runNonFeedback(initialMemory))).map(_.max).runSyncUnsafe(5 seconds).toString

  override def answer2 = "unimplemented"
}
