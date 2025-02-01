package advent2019
import common.Day
import scala.io.Source

class Day1(source: Source) extends Day {
  val masses = source.getLines.map(_.toLong).toList

  private def fuel(mass: Long): Long = mass / 3 - 2

  private def compoundFuel(mass: Long): Long =
    Iterator.iterate(mass)(fuel).takeWhile(_ > 0).drop(1).sum

  override def answer1 = masses.map(fuel).sum.toString

  override def answer2 = masses.map(compoundFuel).sum.toString
}
