package advent2017
import common.Day
import scala.io.Source

class Day2(source: Source) extends Day {
  val input = source.getLines.map(_.split("\\s+") map {_.toInt}).toList

  override def answer1 = input.map{x => x.max - x.min}.sum.toString
  override def answer2 = input.map(_.combinations(2)
    .map{_.sorted}
    .filter{case Array(x,y) => y % x == 0}
    .map{case Array(x,y) => y / x}
    .next).sum.toString
}
