package advent2017
import common.Day
import scala.io.Source

class Day1(source: Source) extends Day {
  val input = source.map(_.asDigit).toList.filter{_ != -1}

  def zipped(offset: Int) = input zip ((input ++ input) drop offset)
  def answer(offset: Int) = zipped(offset).filter{case (x,y) => x == y}.map{_._1}.sum.toString

  override def answer1 = answer(1)
  override def answer2 = answer(input.size/2)
}
