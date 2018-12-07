package advent2017
import common.{Day, Circular}
import Math.min
import scala.io.Source

class Day10(source: Source) extends Day {
  val raw = source.mkString.trim
  val input  = raw.split(",").map(_.toInt).toVector
  val input2 = raw.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)
  val max = 255
  val inputLength = max + 1

  def reverse(in: Circular[Int], start: Int, length: Int): Circular[Int] = {
    val slice = in.slice(start, start + length)
    in.patch(start, slice.reverse, length)
  }

  val initial = (0, 0, new Circular((0 to max).toVector))

  def processInput(input: Vector[Int])(in: (Int, Int, Circular[Int])) = input.foldLeft(in){case ((start, skipSize, in), length) =>
    ((start + skipSize + length) % inputLength, skipSize + 1, reverse(in, start, length))
  }

  val processed = processInput(input)(initial)._3

  override def answer1 = (processed(0) * processed(1)).toString

  val sparseHash = Stream.iterate(initial)(processInput(input2)).drop(64).head._3
  val denseHash = sparseHash.grouped(16).map(_.reduceLeft(_ ^ _))

  override def answer2 = denseHash.map{x => f"${x}%02x"}.mkString("")
}
