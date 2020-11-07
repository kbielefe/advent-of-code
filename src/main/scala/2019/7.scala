package advent2019
import common.Day
import common.Numeric._
import common.Intcode
import scala.io.Source
import monix.eval.Task
import cats.effect.concurrent.MVar
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class Day7(source: Source) extends Day {
  val initialMemory = source.getLines().next().split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap

  def runNonFeedback(memory: Map[Long, Long])(phases: List[Long]): Task[Long] = for {
    inA  <- MVar.of[Task, Long](phases(0))
    aToB <- MVar.of[Task, Long](phases(1))
    bToC <- MVar.of[Task, Long](phases(2))
    cToD <- MVar.of[Task, Long](phases(3))
    dToE <- MVar.of[Task, Long](phases(4))
    outE <- MVar.empty[Task, Long]
    a <- new Intcode("a", inA,  aToB).run(memory, 0, 0).start
    _ <- inA.put(0)
    b <- new Intcode("b", aToB, bToC).run(memory, 0, 0).start
    c <- new Intcode("c", bToC, cToD).run(memory, 0, 0).start
    d <- new Intcode("d", cToD, dToE).run(memory, 0, 0).start
    e <- new Intcode("e", dToE, outE).run(memory, 0, 0).start
    _ <- Task.parSequenceUnordered(List(a.join, b.join, c.join, d.join, e.join))
    result <- outE.take
  } yield result

  def runWithFeedback(memory: Map[Long, Long])(phases: List[Long]): Task[Long] = for {
    eToA <- MVar.of[Task, Long](phases(0))
    aToB <- MVar.of[Task, Long](phases(1))
    bToC <- MVar.of[Task, Long](phases(2))
    cToD <- MVar.of[Task, Long](phases(3))
    dToE <- MVar.of[Task, Long](phases(4))
    a <- new Intcode("a", eToA,  aToB).run(memory, 0, 0).start
    _ <- eToA.put(0)
    b <- new Intcode("b", aToB, bToC).run(memory, 0, 0).start
    c <- new Intcode("c", bToC, cToD).run(memory, 0, 0).start
    d <- new Intcode("d", cToD, dToE).run(memory, 0, 0).start
    e <- new Intcode("e", dToE, eToA).run(memory, 0, 0).start
    _ <- Task.parSequenceUnordered(List(a.join, b.join, c.join, d.join, e.join))
    result <- eToA.take
  } yield result

  override def answer1 = Task.parSequenceUnordered((0 to 4).toList.map(_.toLong).permutations.toList.map(runNonFeedback(initialMemory))).map(_.max).runSyncUnsafe(5.seconds).toString

  override def answer2 = Task.parSequenceUnordered((5 to 9).toList.map(_.toLong).permutations.toList.map(runWithFeedback(initialMemory))).map(_.max).runSyncUnsafe(5.seconds).toString
}
