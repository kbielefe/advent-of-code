package common

import cats.effect._
import cats.effect.{Clock, Resource}
import cats.implicits._

import java.lang.reflect.Constructor

import monix.eval.{Task, TaskApp}
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.duration.NANOSECONDS
import scala.io.Source
import scala.language.existentials

import sys.process._

object Runner extends TaskApp {
  case class ArgumentCountException() extends Exception

  def run(args: List[String]): Task[ExitCode] = {
    val main = for {
      _      <- checkArgCount(args)
      year   <- intArg(args, 0)
      day    <- intArg(args, 1)
      c      <- getClass(year, day)
      legacy <- isLegacy(c)
      constructor <- getConstructor(c)
      input   = getInput(year, day)
      run1   <- runPart(args, 1)
      run2   <- runPart(args, 2)
      task1   = if (legacy) legacyTask(constructor, input, 1) else newTask(constructor, input, 1)
      task2   = if (legacy) legacyTask(constructor, input, 2) else newTask(constructor, input, 2)
      fiber  <- if (run1) timeTask(year, day, 1, task1).start else Task.unit.start
      _      <- if (run2) timeTask(year, day, 2, task2) else Task.unit
      _      <- fiber.join
    } yield ExitCode.Success

    main.onErrorRecoverWith{
      case e: ArgumentCountException => printError(usage)
      case e: NumberFormatException  => printError(usage)
      case e: ClassNotFoundException => printError(notFound)
      case e: Throwable              => printError(s"${e.toString}:\n ${e.getStackTrace.mkString("\n")}")
    }
  }

  def timeTask(year: Int, day: Int, part: Int, task: Task[String])(implicit clock: Clock[Task]): Task[Unit] = for {
    start  <- clock.monotonic(NANOSECONDS)
    result <- task
    end    <- clock.monotonic(NANOSECONDS)
    runtime = f"${(end - start).toDouble / 1000000.0}%2.1f ms"
    _      <- Task{Seq("/usr/bin/notify-send", "-t", "10000", s"$year/$day part $part<br/>$result<br/>$runtime").run()}
    _      <- Task{println(s"$year/$day part $part\n$result\n$runtime")}
  } yield ()

  def legacyTask(constructor: Constructor[_], input: Resource[Task, Source], part: Int): Task[String] = input.use{source =>
    val puzzle = constructor.newInstance(source).asInstanceOf[Day]
    if (part == 1)
      Task{puzzle.answer1}
    else
      Task{puzzle.answer2}
  }

  def newTask[A, B, C](constructor: Constructor[_], input: Resource[Task, Source], part: Int) = for {
    puzzle <- Task{constructor.newInstance().asInstanceOf[DayTask[A, B, C]]}
    parsed <- puzzle.input(Observable.fromIterator(input.map(_.getLines())))
    result <- if (part == 1) puzzle.part1(parsed).map(_.toString) else puzzle.part2(parsed).map(_.toString)
  } yield result

  private def checkArgCount(args: List[String]): Task[Unit] =
    if (args.size < 2 || args.size > 3) {
        Task.raiseError(ArgumentCountException())
    } else {
      Task.unit
    }

  private def intArg(args: List[String], position: Int): Task[Int] = Task{
    args(position).toInt
  }

  private def runPart(args: List[String], part: Int) =
    Task{args.size < 3 || args(2).toInt == part}

  private def printError(msg: String) = Task{
    println(msg)
    Seq("/usr/bin/notify-send", "-t", "10000", msg).run()
  }.as(ExitCode.Error)

  private def usage = "Usage: <year> <day> [<part>]"

  private def notFound = "Specified puzzle not found."

  private def getClass(year: Int, day: Int) = Task{
    Class.forName(s"advent$year.Day$day")
  }

  private def isLegacy(c: Class[_]) =
    Task{c.getInterfaces.map(_.getSimpleName).head == "Day"}

  private def getConstructor(c: Class[_]) = Task{c.getConstructors()(0)}

  private def getInput(year: Int, day: Int): Resource[Task, Source] = {
    val path = new java.io.File(s"./src/main/resources/$year/$day.txt").getCanonicalPath
    Resource.make(Task{Source.fromFile(path)}.onErrorFallbackTo(Task{Source.fromString("")}))(source => Task{source.close()})
  }
}
