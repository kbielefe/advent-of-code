package advent2018
import common.Day
import scala.io.Source

class Day2(source: Source) extends Day {
  val input = source.getLines.toList
  val counts = input map {_.groupBy(identity).map{_._2.length}.toList}
  def appearsN(n: Int): Int = counts count {_ contains n}

  override def answer1 = (appearsN(2) * appearsN(3)).toString

  def differsByOne(x: List[String]): Boolean = {
    val List(first, second) = x
    val differenceCount = first zip second count {case (x, y) => x != y}
    differenceCount == 1
  }

  def sameCharacters(x: List[String]): String = {
    val List(first, second) = x
    val same = first zip second filter {case (x, y) => x == y}
    same.map(_._1).mkString("")
  }

  override def answer2 = input.combinations(2).find(differsByOne).map(sameCharacters).toString
}
