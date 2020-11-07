package advent2017
import common.Day
import scala.io.Source

class Day5(source: Source) extends Day {
  val input = source.getLines().map(_.toInt).toVector

  def results(f: Int => Int) = LazyList.iterate((0, 0, input)){case (step, index, input) =>
    val offset = input(index)
    val newInput = input.updated(index, f(offset))
    (step + 1, index + offset, newInput)
  }

  def answer(f: Int => Int) = results(f).dropWhile(x => x._2 >= 0 && x._2 < input.size).head._1.toString

  override def answer1 = answer(_ + 1)
  override def answer2 = answer(offset => if (offset >= 3) offset - 1 else offset + 1)
}
