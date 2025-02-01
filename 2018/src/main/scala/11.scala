package advent2018
import common.{Day, Dynamic, Visualize}
import scala.io.Source
import monix.tail.Iterant
import monix.eval.Task
import cats.implicits._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class Day11(source: Source) extends Day {

  val serialNumber = source.mkString.trim.toInt

  def hundredsDigit(n: Int): Int = {
    n / 100 % 10
  }

  def powerLevel(x: Int, y: Int): Int = {
    val rackID = x + 10
    hundredsDigit((rackID * y + serialNumber) * rackID) - 5
  }

  def grid: List[List[Int]] = (1 to 300).toList map {y => (1 to 300).toList.map{x => powerLevel(x, y)}}

  lazy val cumulative: Vector[Vector[Int]] = Dynamic.cumulativeSums(grid).map{_.toVector}.toVector

  def gridPower(x: Int, y: Int, size: Int): ((Int, Int, Int), Int) = {
    val power = cumulative(y + size - 2)(x + size - 2) -
    (if (y >= 2) cumulative(y - 2)(x + size - 2) else 0) -
    (if (x >= 2) cumulative(y + size - 2)(x - 2) else 0) +
    (if (x >= 2 && y >= 2) cumulative(y - 2)(x - 2) else 0)
    ((x, y, size), power)
  }

  def answer(sizes: Iterant[Task, Int]): String = {
    val gridPowers: Iterant[Task, ((Int, Int, Int), Int)] = for {
      size <- sizes
      x    <- Iterant[Task].range(1, 302 - size)
      y    <- Iterant[Task].range(1, 302 - size)
    } yield gridPower(x, y, size)
    val buffered = gridPowers.bufferTumbling(8).mapEval(b => Task.gather(b map {x => Task(x)}))
    val max = gridPowers.maxByL{_._2}.map{_.get._1.toString}
    max.runSyncUnsafe()
  }

  override def answer1: String = answer(Iterant[Task].of(3))
  override def answer2: String = answer(Iterant[Task].range(1, 301))
}
