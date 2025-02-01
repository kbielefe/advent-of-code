package advent2017
import common.Day
import scala.io.Source

class Day12(source: Source) extends Day {
  val lines = source.getLines
  val input = lines.map(_ split " <-> ").map{x => (x(0).toInt, x(1).split(", ").map(_.trim.toInt).toSet)}.toMap

  def group(root: Int): Set[Int] = {
    val closure = Stream.iterate(Set(root)){_ flatMap {x => input(x) + x}}
    val sizes = closure map {_.size}
    val sizesEqual = (sizes zip (sizes drop 1)).map{case (x, y) => x == y}
    (closure zip sizesEqual).dropWhile(!_._2).map(_._1).head
  }

  override def answer1 = group(0).size.toString
  override def answer2 = input.keySet.map(group).size.toString
}
