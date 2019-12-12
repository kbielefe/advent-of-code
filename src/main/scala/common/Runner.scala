package common
import scala.io.Source
import scala.util.{Try, Success, Failure}
import sys.process._
import cats.effect.{Clock, Resource}
import monix.eval.Task
import monix.reactive.Observable
import monix.execution.Scheduler
import scala.concurrent.duration.NANOSECONDS

object Runner {
  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      println("Call using year and day, i.e. 2018 1")
      println("Optionally, as a third argument, specify a single part to run")
      return
    }

    val year = args(0)
    val day  = args(1)

    def runIf(n: Int, answer: => String): Unit = {
      if (args.size < 3 || args(2).toInt == n) {
        val startTime = System.nanoTime()
        val storedAnswer = answer
        println(storedAnswer)
        val runtime = f"${(System.nanoTime() - startTime).toDouble / 1000000.0}%2.1f ms"
        println(runtime)
        val cmd = Seq("/usr/bin/notify-send", "-t", "10000", s"$year/$day part $n<br/>$storedAnswer<br/>$runtime")
        cmd.lineStream
      }
    }

    def runDay[T](c: Class[T]): Unit = {
      val source = Source.fromResource(s"$year/$day.txt")
      val constructor = c.getConstructors()(0)
      val puzzle = constructor.newInstance(source).asInstanceOf[Day]
      runIf(1, puzzle.answer1)
      runIf(2, puzzle.answer2)
    }

    def runDayTask[T, A, B, C](c: Class[T])(implicit clock: Clock[Task]): Unit = {
      val constructor = c.getConstructors()(0)
      val puzzle = constructor.newInstance().asInstanceOf[DayTask[A, B, C]]
      val inputResource = Resource.make(Task{Source.fromResource(s"$year/$day.txt")})(source => Task{source.close()}).map(_.getLines)
      val inputTask = puzzle.input(Observable.fromIterator(inputResource))
      def runPart(part: Int) = args.size < 3 || args(2).toInt == part

      def runTask[D](part: Int, task: Task[D]): Task[Unit] = for {
        start  <- clock.monotonic(NANOSECONDS)
        result <- task.map(_.toString)
        end    <- clock.monotonic(NANOSECONDS)
        runtime = f"${(end - start).toDouble / 1000000.0}%2.1f ms"
        cmd     = Seq("/usr/bin/notify-send", "-t", "10000", s"$year/$day part $part<br/>$result<br/>$runtime")
        _      <- Task{println(s"$year/$day part $part\n$result\n$runtime")}
        _      <- Task{cmd.run()}
      } yield ()

      val task = for {
        input <- inputTask
        part1 <- if (runPart(1)) runTask(1, puzzle.part1(input)).start else Task.unit.start
        part2 <- if (runPart(2)) runTask(2, puzzle.part2(input)).start else Task.unit.start
        _     <- Task.gatherUnordered(Seq(part1.join, part2.join))
      } yield ()
      implicit val scheduler = Scheduler.computation()
      task.runSyncUnsafe()
    }


    val className = s"advent$year.Day$day"
    Try(Class.forName(className)) map {c =>
      val traitName = c.getInterfaces.map(_.getSimpleName).head
      if (traitName == "Day") {
        runDay(c)
      } else if (traitName == "DayTask") {
        runDayTask(c)
      } else {
        println(s"Puzzle implements unknown trait $traitName")
      }
    } recover {case e =>
      println(s"Error constructing class $className:\n${e.toString}")
    }
  }
}
