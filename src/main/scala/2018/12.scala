package advent2018
import common.{Day, Dynamic}
import scala.io.Source

class Day12(source: Source) extends Day {

  val input = source.getLines
  val initialStateRegex = """initial state: ([#.]+)""".r

  val initialState = input.next match {
    case initialStateRegex(state) => toBooleanList(state)
  }

  def toBooleanList(s: String): List[Boolean] = s.toList.map{_ == '#'}
  input.next // skip blank line

  val ruleRegex = """([#.]+)\s*=>\s*([#.])""".r
  def toRule(s: String): (List[Boolean], Boolean) = s match {
    case ruleRegex(pattern, result) => (toBooleanList(pattern), toBooleanList(result).head)
  }

  val rules: Map[List[Boolean], Boolean] = input.map(toRule).toMap

  def growGeneration(in: List[(Boolean, Int)]): List[(Boolean, Int)] = {
    val firstNumber = in.head._2
    val lastNumber = in.last._2
    val fourFalses = List(false, false, false, false)
    val leftPadding = fourFalses zip ((firstNumber - 4) to (firstNumber - 1))
    val rightPadding = fourFalses zip ((lastNumber + 1) to (lastNumber + 4))
    (leftPadding ++ in ++ rightPadding).sliding(5).map{pattern =>
      val hasPlant = rules.getOrElse(pattern map {_._1}, false)
      val number = pattern.drop(2).head._2
      (hasPlant, number)
    }.toList.dropWhile(!_._1).reverse.dropWhile(!_._1).reverse
  }

  def answer(in: List[(Boolean, Int)]): String = {
    in.filter(_._1).map(_._2).sum.toString
  }

  override def answer1: String = answer(Iterator.iterate(initialState.zipWithIndex)(growGeneration).drop(20).next)
  override def answer2: String = Dynamic.detectCycle(Iterator.iterate(initialState.zipWithIndex)(growGeneration)).toString
}
