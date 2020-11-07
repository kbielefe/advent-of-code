package advent2018
import common.{Day, Dynamic}
import scala.io.Source

class Day1(source: Source) extends Day {
  val input = source.getLines().map(_.toInt).toList
  def repeated = Iterator.continually(input).flatten.scanLeft(0){_ + _}

  override def answer1 = input.sum.toString
  override def answer2 = Dynamic.detectCycle(repeated).map{_._3}.get.toString
}
