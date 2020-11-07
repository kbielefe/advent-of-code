package advent2017
import common.Day
import scala.io.Source

class Day13(source: Source) extends Day {
  val lines = source.getLines()
  val input = lines.map(_ split ": ").map{x => x(0).toInt -> x(1).toInt}.toMap

  def caught(delay: Int)(layer: Int): Boolean = {
    val timeEnteredLayer = layer + delay
    val scanLength = input(layer) * 2 - 2
    val firewallPosition = timeEnteredLayer % scanLength
    firewallPosition == 0
  }

  def severity(layer: Int): Int = input(layer) * layer

  def caughtOnAnyLayer(delay: Int): Boolean = input.keys.exists{layer => caught(delay)(layer)}

  override def answer1 = input.keys.filter{caught(0)}.map(severity).sum.toString
  override def answer2 = LazyList.from(1).filterNot{caughtOnAnyLayer}.head.toString
}
