package common
import scala.io.Source
import scala.util.{Try, Success, Failure}

object Runner {
  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      println("Call using year and day, i.e. 2018 1")
      return
    }
    val year = args(0)
    val day  = args(1)
    val source = Source.fromResource(s"$year/$day.txt")
    val className = s"advent$year.Day$day"
    val answers = Try(Class.forName(className)) map {c =>
      val constructor = c.getConstructors()(0)
      val puzzle = constructor.newInstance(source).asInstanceOf[Day]
      puzzle.answer1 + "\n" + puzzle.answer2
    } 
    println(answers getOrElse s"Unable to find class $className")
  }
}
