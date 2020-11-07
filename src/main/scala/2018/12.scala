package advent2018
import common.{Day, Dynamic}
import scala.io.Source

class Day12(source: Source) extends Day {

  val input = source.getLines()
  val initialStateRegex = """initial state: ([#.]+)""".r

  val initialState: Set[Int] = input.next() match {
    case initialStateRegex(state) => toState(state)
  }

  def toBooleanList(s: String): List[Boolean] = s.toList.map{_ == '#'}

  def toState(s: String): Set[Int] = toBooleanList(s).zipWithIndex.filter{_._1}.map{_._2}.toSet

  input.next() // skip blank line

  val ruleRegex = """([#.]+)\s*=>\s*([#.])""".r
  def toRule(s: String): (List[Boolean], Boolean) = s match {
    case ruleRegex(pattern, result) => (toBooleanList(pattern), toBooleanList(result).head)
  }

  val rules: Map[List[Boolean], Boolean] = input.map(toRule).toMap

  def growGeneration(in: Set[Int]): Set[Int] = {
    val pots: Set[Int] = in flatMap {pot => (pot - 4) to (pot + 4)}
    pots.map{pot =>
      val pattern = (-2 to 2).toList map {pos => in contains (pot + pos)}
      rules.get(pattern) flatMap {if (_) Some(pot) else None}
    }.flatten
  }

  def answer(in: Set[Int]): String = in.sum.toString

  def generationIterator: Iterator[Set[Int]] = Iterator.iterate(initialState)(growGeneration)

  override def answer1: String = answer(generationIterator.drop(20).next())
  override def answer2: String = {
    val (stableTime, _, _) = Dynamic.detectCycle(generationIterator.map{x => x.map{_ - x.min}}).get
    val stablePositions = generationIterator.drop(stableTime).next()
    stablePositions.map{50000000000L - stableTime + _}.sum.toString
  }
}
