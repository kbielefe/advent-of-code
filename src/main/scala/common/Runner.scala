package common
import scala.io.Source
import scala.util.{Try, Success, Failure}
import sys.process._

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
        println(answer)
        val runtime = f"${(System.nanoTime() - startTime).toDouble / 1000000.0}%2.1f ms"
        println(runtime)
        val cmd = Seq("/usr/bin/notify-send", "-t", "10000", s"$year/$day part $n<br/>$answer<br/>$runtime")
        cmd.lineStream
      }
    }

    val source = Source.fromResource(s"$year/$day.txt")
    val className = s"advent$year.Day$day"
    val puzzle = Try(Class.forName(className)) map {c =>
      val constructor = c.getConstructors()(0)
      constructor.newInstance(source).asInstanceOf[Day]
    }
    puzzle match {
      case Success(p) =>
          runIf(1, p.answer1)
          runIf(2, p.answer2)
      case Failure(e) => println(s"Error constructing class $className:\n${e.toString}")
    }
  }
}
