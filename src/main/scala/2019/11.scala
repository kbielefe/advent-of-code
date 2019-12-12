package advent2019
import common.Day
import common.Intcode
import scala.io.Source
import monix.eval.Task
import cats.effect.concurrent.{MVar, Ref}
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class Day11(source: Source) extends Day {
  val initialMemory = source.getLines.next.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap

  val black = 0L
  val white = 1L

  def turn(direction: Char, turnDirection: Long): Char = (direction, turnDirection) match {
    case ('^', 0L)  => '<'
    case ('^', 1L) => '>'
    case ('>', 0L)  => '^'
    case ('>', 1L) => 'v'
    case ('v', 0L)  => '>'
    case ('v', 1L) => '<'
    case ('<', 0L)  => 'v'
    case ('<', 1L) => '^'
  }

  def move(position: (Int, Int), direction: Char): (Int, Int) = (position, direction) match {
    case ((x, y), '^') => (x, y + 1)
    case ((x, y), 'v') => (x, y - 1)
    case ((x, y), '<') => (x - 1, y)
    case ((x, y), '>') => (x + 1, y)
  }

  def robot(
      sensor: MVar[Task, Long],
      command: MVar[Task, Long],
      output: Ref[Task, Map[(Int, Int), Long]],
      position: (Int, Int),
      direction: Char): Task[Unit] = for {
    painted       <- output.get
    _             <- sensor.put(painted.getOrElse(position, black))
    paintColor    <- command.take
    _             <- output.set(painted + (position -> paintColor))
    turnDirection <- command.take
    newDirection  =  turn(direction, turnDirection)
    newPosition   =  move(position, newDirection)
    _             <- robot(sensor, command, output, newPosition, newDirection)
  } yield ()

  def run(initialColor: Long) = for {
    in      <- MVar.empty[Task, Long]
    out     <- MVar.empty[Task, Long]
    painted <- Ref.of[Task, Map[(Int, Int), Long]](Map((0, 0) -> initialColor))
    _       <- robot(in, out, painted, (0, 0), '^').start
    program <- new Intcode("robot", in, out).run(initialMemory, 0, 0)
    result  <- painted.get
  } yield result

  def draw(painted: Map[(Int, Int), Long]): String = {
    val minX = painted.keys.map(_._1).min
    val minY = painted.keys.map(_._2).min
    val maxX = painted.keys.map(_._1).max
    val maxY = painted.keys.map(_._2).max
    (maxY to minY by -1).map(y =>
      (minX to maxX).map(x => if (painted.getOrElse((x, y), black) == black) ' ' else '█').mkString
    ).mkString("\n")
  }

  override def answer1 = run(black).runSyncUnsafe(5 seconds).size.toString

  override def answer2 = draw(run(white).runSyncUnsafe(5 seconds))
}
