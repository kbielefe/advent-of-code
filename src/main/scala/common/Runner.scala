package common
import scala.io.Source

object Runner {
  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("2018/1.txt")
    val day = new advent2018.Day1(source)
    println(day.answer1)
  }
}
