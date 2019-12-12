package advent2019
import common.DayTask
import common.Intcode
import monix.eval.Task
import monix.reactive.Observable
import cats.effect.concurrent.{MVar, Ref}

class Day11 extends DayTask[Map[Long, Long], Int, String] {
  override def input(lines: Observable[String]) = lines.headL.map{line =>
    line.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap
  }

  val Black = 0L
  val White = 1L

  val Left  = 0L
  val Right = 1L

  def turn(direction: Char, turnDirection: Long): Char = (direction, turnDirection) match {
    case ('^', Left)  => '<'
    case ('^', Right) => '>'
    case ('>', Left)  => '^'
    case ('>', Right) => 'v'
    case ('v', Left)  => '>'
    case ('v', Right) => '<'
    case ('<', Left)  => 'v'
    case ('<', Right) => '^'
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
    _             <- sensor.put(painted.getOrElse(position, Black))
    paintColor    <- command.take
    _             <- output.set(painted + (position -> paintColor))
    turnDirection <- command.take
    newDirection  =  turn(direction, turnDirection)
    newPosition   =  move(position, newDirection)
    _             <- robot(sensor, command, output, newPosition, newDirection)
  } yield ()

  def run(initialMemory: Map[Long, Long], initialColor: Long) = for {
    in      <- MVar.empty[Task, Long]
    out     <- MVar.empty[Task, Long]
    painted <- Ref.of[Task, Map[(Int, Int), Long]](Map((0, 0) -> initialColor))
    _       <- robot(in, out, painted, (0, 0), '^').start
    program <- new Intcode(if (initialColor == Black) "black" else "white", in, out).run(initialMemory, 0, 0)
    result  <- painted.get
  } yield result

  def draw(painted: Map[(Int, Int), Long]): String = {
    val minX = painted.keys.map(_._1).min
    val minY = painted.keys.map(_._2).min
    val maxX = painted.keys.map(_._1).max
    val maxY = painted.keys.map(_._2).max
    (maxY to minY by -1).map(y =>
      (minX to maxX).map(x => if (painted.getOrElse((x, y), Black) == Black) ' ' else '█').mkString
    ).mkString("\n")
  }

  override def part1(initialMemory: Map[Long, Long]) = run(initialMemory, Black).map(_.size)

  override def part2(initialMemory: Map[Long, Long]) = run(initialMemory, White).map(draw)
}
