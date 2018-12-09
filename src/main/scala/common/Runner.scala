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
      case "2015/1"  => new advent2015.Day1(source)
      case "2015/2"  => new advent2015.Day2(source)
      case "2015/3"  => new advent2015.Day3(source)
      case "2015/4"  => new advent2015.Day4(source)
      case "2017/1"  => new advent2017.Day1(source)
      case "2017/2"  => new advent2017.Day2(source)
      case "2017/3"  => new advent2017.Day3(source)
      case "2017/4"  => new advent2017.Day4(source)
      case "2017/5"  => new advent2017.Day5(source)
      case "2017/6"  => new advent2017.Day6(source)
      case "2017/7"  => new advent2017.Day7(source)
      case "2017/8"  => new advent2017.Day8(source)
      case "2017/9"  => new advent2017.Day9(source)
      case "2017/10" => new advent2017.Day10(source)
      case "2017/11" => new advent2017.Day11(source)
      case "2017/12" => new advent2017.Day12(source)
      case "2017/13" => new advent2017.Day13(source)
      case "2017/14" => new advent2017.Day14(source)
      case "2017/15" => new advent2017.Day15(source)
      case "2017/16" => new advent2017.Day16(source)
      case "2017/17" => new advent2017.Day17(source)
      case "2017/18" => new advent2017.Day18(source)
      case "2017/19" => new advent2017.Day19(source)
      case "2017/20" => new advent2017.Day20(source)
      case "2017/21" => new advent2017.Day21(source)
      case "2017/22" => new advent2017.Day22(source)
      case "2017/23" => new advent2017.Day23(source)
      case "2017/24" => new advent2017.Day24(source)
      case "2017/25" => new advent2017.Day25(source)
      case "2018/1"  => new advent2018.Day1(source)
      case "2018/2"  => new advent2018.Day2(source)
      case "2018/3"  => new advent2018.Day3(source)
      case "2018/4"  => new advent2018.Day4(source)
      case "2018/5"  => new advent2018.Day5(source)
      case "2018/6"  => new advent2018.Day6(source)
      case "2018/7"  => new advent2018.Day7(source)
      case "2018/8"  => new advent2018.Day8(source)
      case "2018/9"  => new advent2018.Day9(source)
      case _         => println("Puzzle not available"); return
    }
    println(day.answer1)
    println(day.answer2)
  }
}
