package advent2019
import common.DayTask
import common.Intcode
import monix.eval.Task
import monix.reactive.Observable
import cats.effect.concurrent.{MVar, Ref}

class Day13 extends DayTask[Map[Long, Long], Int, Long] {
  override def input(lines: Observable[String]) = lines.headL.map{line =>
    line.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap
  }

  def arcade(input: MVar[Task, Long], screenRef: MVar[Task, Map[(Long, Long), Long]], scoreRef: MVar[Task, Long], readyToMove: MVar[Task, Boolean]): Task[Unit] = for {
    x      <- input.take
    y      <- input.take
    block  <- input.take
    screen <- screenRef.take
    score  <- scoreRef.take
    newScreen = if (x == -1 && y == 0) screen else screen.updated((x, y), block)
    newScore  = if (x == -1 && y == 0) block else score
    _ <- screenRef.put(newScreen)
    _ <- scoreRef.put(newScore)
    _ <- draw(newScreen, newScore)
    _ <- if (block == 4) readyToMove.put(true) else Task.unit
    _ <- arcade(input, screenRef, scoreRef, readyToMove)
  } yield ()

  def draw(screen: Map[(Long, Long), Long], score: Long): Task[Unit] = {
    val minX = screen.keys.map(_._1).min
    val maxX = screen.keys.map(_._1).max
    val minY = screen.keys.map(_._2).min
    val maxY = screen.keys.map(_._2).max

    val output = (minY to maxY).map{y =>
      (minX to maxX).map{x =>
        screen.getOrElse((x, y), 0) match {
          case 0 => ' '
          case 1 => '#'
          case 2 => 'X'
          case 3 => '-'
          case 4 => '*'
        }
      }.mkString
    }.mkString("\n")
    Task{println(s"\nScore: $score\n\n$output")}
  }

  def run(initialMemory: Map[Long, Long]) = for {
    in      <- MVar.empty[Task, Long]
    out     <- MVar.empty[Task, Long]
    ready   <- MVar.empty[Task, Boolean]
    screen  <- MVar.of[Task, Map[(Long, Long), Long]](Map.empty)
    score   <- MVar.of[Task, Long](0)
    _       <- arcade(out, screen, score, ready).start
    _       <- player(screen, in, ready).start
    program <- new Intcode("arcade", in, out).run(initialMemory, 0, 0)
    finalScreen <- screen.take
    finalScore  <- score.take
  } yield (finalScreen, finalScore)

  def player(screenRef: MVar[Task, Map[(Long, Long), Long]], joystick: MVar[Task, Long], readyToMove: MVar[Task, Boolean]): Task[Unit] = for {
    ready  <- readyToMove.take
    screen <- screenRef.read
    char   <- calculateMove(screen)
    _      <- joystick.put(char)
    _      <- player(screenRef, joystick, readyToMove)
  } yield ()

  def calculateMove(screen: Map[(Long, Long), Long]) = Task{
    val ballX = screen.find{case ((x, y), block) => block == 4}.map(_._1._1).getOrElse(-1L)
    val paddleX = screen.find{case ((x, y), block) => block == 3}.map(_._1._1).getOrElse(-1L)
    if (ballX < paddleX)
      -1
    else if (ballX > paddleX)
      1
    else
      0
  }

  override def part1(initialMemory: Map[Long, Long]) = run(initialMemory).map(_._1.values.count(_ == 2))

  override def part2(initialMemory: Map[Long, Long]) = run(initialMemory.updated(0, 2)).map(_._2)
}
