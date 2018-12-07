package common
import scala.io.Source

object Runner {
  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      println("Call using year and day, i.e. 2018 1")
      return
    }
    val date = s"${args(0)}/${args(1)}"
    val source = Source.fromResource(s"${date}.txt")
    val day = date match {
      case "2015/1" => new advent2015.Day1(source)
      case "2015/2" => new advent2015.Day2(source)
      case "2015/3" => new advent2015.Day3(source)
      case "2015/4" => new advent2015.Day4(source)
      case "2018/1" => new advent2018.Day1(source)
      case "2018/2" => new advent2018.Day2(source)
      case "2018/3" => new advent2018.Day3(source)
      case "2018/4" => new advent2018.Day4(source)
      case "2018/5" => new advent2018.Day5(source)
      case "2018/6" => new advent2018.Day6(source)
      case "2018/7" => new advent2018.Day7(source)
      case _        => println("Puzzle not available"); return
    }
    println(day.answer1)
    println(day.answer2)
  }
}
