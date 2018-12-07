package advent2017
import common.Day
import scala.io.Source

class Day24(source: Source) extends Day {
  type Bridge     = List[(Int, Int)]
  type Components =  Set[(Int, Int)]

  val rawInput = source.getLines.map{_.split("/").map(_.toInt)}.toSet

  val input = rawInput.flatMap{case Array(x, y) => Set(Array(x, y), Array(y, x))}.groupBy{_(0)}.mapValues(_.map(_.sorted).map{case Array(x, y) => (x, y)})

  def removeElement(element: (Int, Int), input: Map[Int, Components]): Map[Int, Components] = {
    val (x, y) = element
    val newFirstSet = input(x) - element
    val afterFirstRemoved = if (newFirstSet.isEmpty) input - x else input + (x -> newFirstSet)
    val newSecondSet = input(y) - element
    if (newSecondSet.isEmpty) afterFirstRemoved - y else afterFirstRemoved + (y -> newSecondSet)
  }

  def bridges(input: Map[Int, Components], acc: Bridge = List.empty[(Int, Int)])(port: Int): Stream[Bridge] = {
    if (input contains port) {
      input(port).toStream
        .flatMap{case (x, y) =>
          val next = bridges(removeElement((x, y), input), (x, y) :: acc) _
          if (x == port) next(y) else next(x)
        }
        } else {
          Stream(acc.reverse)
        }
  }

  def strength(bridge: Bridge): Int = bridge.map{case (x, y) => x + y}.sum

  val memBridge = bridges(input)(0).map(x => (x.length, strength(x)))

  override def answer1 = memBridge.maxBy(_._2)._2.toString
  override def answer2 = memBridge.max._2.toString
}
