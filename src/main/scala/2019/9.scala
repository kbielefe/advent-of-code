package advent2019
import common.Day
import common.Numeric._
import common.Intcode
import scala.io.Source
import monix.eval.Task
import cats.effect.concurrent.MVar
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class Day9(source: Source) extends Day {
  val initialMemory = source.getLines.next.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap

  def runBoost(input: Long) = for {
    in     <- MVar.of[Task, Long](input)
    out    <- MVar.empty[Task, Long]
    boost  <- new Intcode("boost", in, out).run(initialMemory, 0, 0)
    result <- out.take
  } yield result

  override def answer1 = runBoost(1).runSyncUnsafe(5 seconds).toString

  override def answer2 = runBoost(2).runSyncUnsafe(5 seconds).toString
}
