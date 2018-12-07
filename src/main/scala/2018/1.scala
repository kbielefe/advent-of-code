package advent2018
import common.Day
import scala.io.Source

class Day1(source: Source) extends Day {
  val input = source.getLines.map(_.toInt).toList
  def repeated = Iterator.continually(input).flatten.scanLeft(0){_ + _}
  val cumulative = repeated.scanLeft(Set.empty[Int]){case (seen, next) => seen + next}

  override def answer1 = input.sum.toString
  override def answer2 = (cumulative zip repeated).dropWhile{case (seen, next) => !seen.contains(next)}.next._2.toString
}
