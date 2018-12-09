package advent2018
import common.{Day, LinkedCircular}
import scala.io.Source

class Day9(source: Source) extends Day {

  val input = source.mkString.trim
  val regex = """(\d+) players; last marble is worth (\d+) points""".r
  val (playerCount, lastScore) = input match {
    case regex(x, y) => (x.toInt, y.toInt)
  }

  def moves: Iterator[(Int, Int, Long, LinkedCircular[Int])] = {
    Iterator.iterate((0, 0, 0L, LinkedCircular(Map(0 -> (0, 0))))){case (current, number, _, marbles) =>
      val newNumber = number + 1
      val multOf23 = (newNumber % 23) == 0
      val newCurrent = if (multOf23) marbles.move(current, -6) else newNumber
      val removed = marbles.move(current, -7)
      val insertAfter = marbles.move(current, 1)
      val newMarbles = if (multOf23)
          marbles.delete(removed)
        else
          marbles.insertAfter(insertAfter, newNumber)
      val points = if (multOf23) removed + newNumber else 0
      (newCurrent, newNumber, points, newMarbles)
    }
  }

  def maxScore(lastScore: Int): Long = {
    val playerTurn = Iterator.continually(1 to playerCount).flatten take (lastScore + 1)
    val scores = moves drop 1 map {_._3}
    val playerScores = playerTurn.zip(scores).foldLeft(Map.empty[Int, Long]){case (result, (player, score)) =>
      val previousScore = result.getOrElse(player, 0L)
      result + (player -> (score + previousScore))
    }
    playerScores.maxBy{_._2}._2
  }

  override def answer1: String = maxScore(lastScore).toString
  override def answer2: String = maxScore(lastScore * 100).toString
}
