package common
import scala.io.Source
import scala.util.{Try, Success, Failure}

object Runner {
  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      println("Call using year and day, i.e. 2018 1")
      println("Optionally, as a third argument, specify a single part to run")
      return
    }

    def runIf(n: Int, answer: => String): Unit = {
      if (args.size < 3 || args(2).toInt == n) {
        val startTime = System.nanoTime()
        println(answer)
        println(f"${(System.nanoTime() - startTime).toDouble / 1000000.0}%2.1f ms")
      }
    }

    val year = args(0)
    val day  = args(1)
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
