package advent2017
import common.Day
import scala.io.Source

class Day4(source: Source) extends Day {
  val input = source.getLines.map(_.split(' ')).toList

  val noDups = (words: Array[String]) => words.distinct.size == words.size
  override def answer1 = input.filter(noDups).size.toString
  override def answer2 = input.map{_ map {_.sorted}}.filter(noDups).size.toString
}
