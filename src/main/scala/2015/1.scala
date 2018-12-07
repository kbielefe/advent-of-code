package advent2015
import common.Day
import scala.io.Source

class Day1(source: Source) extends Day {
  val input = source.mkString.trim

  val floors = input.scanLeft(0){case (floor, char) => if (char == '(') floor + 1 else floor - 1}

  override def answer1 = floors.last.toString

  override def answer2 = floors.indexWhere(_ < 0).toString
}
