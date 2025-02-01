package advent2017
import common.Day
import scala.io.Source
import Math.max

class Day9(source: Source) extends Day {
  val input = source.mkString

  val nonCanceled = input.replaceAll("!.", "")

  val isGarbage = nonCanceled.scanLeft(false){case (garbage, char) =>
    if (char == '>') false else char == '<' || garbage
  }

  val nonGarbage = (isGarbage zip nonCanceled) filterNot {_._1} map {_._2} filter {_ != '<'}

  val braces = nonGarbage filter {_ != ','}

  val nesting = braces.scanLeft(0){case (nesting, char) => if (char == '{') nesting + 1 else nesting - 1}.drop(1)

  override def answer1 = (nesting zip braces).filter{_._2 == '{'}.map(_._1).sum.toString
  override def answer2 = (isGarbage zip nonCanceled).filter{case (isGarbage, char) => isGarbage && (char != '>')}.size.toString
}
