package advent2015
import common.Day
import scala.io.Source

class Day5(source: Source) extends Day {
  val input = source.getLines.toList

  def containsThreeVowels(in: String): Boolean = in.filter{"aeiou" contains _}.size >= 3

  def containsDouble(in: String): Boolean = in.sliding(2).exists{x => x(0) == x(1)}

  def containsForbiddenStrings(in: String): Boolean = in.sliding(2).exists{Set("ab", "cd", "pq", "xy") contains _}

  def niceString(in: String): Boolean = containsThreeVowels(in) && containsDouble(in) && !containsForbiddenStrings(in)

  def hasNonOverlappingPairs(in: String): Boolean = {
    val pairsWithIndex = in.sliding(2).zipWithIndex.toList
    val indicesOnly = pairsWithIndex.groupBy{_._1}.values.map{_.map{_._2}}
    indicesOnly.exists{x => x.max - x.min > 1}
  }

  def hasRepeatBetween(in: String): Boolean = in.sliding(3).exists{x => x(0) == x(2)}

  def niceStringPart2(in: String): Boolean = hasNonOverlappingPairs(in) && hasRepeatBetween(in)

  override def answer1 = input.count(niceString).toString

  override def answer2 = input.count(niceStringPart2).toString
}
