package advent2017
import common.Day
import scala.io.Source

class Day16(source: Source) extends Day {
  val input = source.mkString.split(",").map(_.trim)

  def spin(amount: Int, order: String): String =
    order.takeRight(amount) ++ order.take(order.size - amount)

  def exchange(positions: String, order: String): String = {
    val Array(x, y) = positions.split("/").map(_.toInt)
    order.updated(x, order(y)).updated(y, order(x))
  }

  def partner(programs: String, order: String): String = {
    val x = programs(0)
    val y = programs(2)
    order.updated(order.indexOf(x), y).updated(order.indexOf(y), x)
  }

  val initial = ('a' to 'p').mkString

  def danceMoves(initial: String): String = input.foldLeft(initial){case (order, move) =>
    move.head match {
      case 's' => spin(move.tail.toInt, order)
      case 'x' => exchange(move.tail, order)
      case 'p' => partner(move.tail, order)
    }
  }

  override def answer1 = danceMoves(initial).toString

  def iterations = Iterator.iterate(initial)(danceMoves)

  def loopLength = iterations.drop(1).takeWhile(_ != initial).size + 1

  override def answer2 = iterations.drop(1000000000 % loopLength).next.toString
}
