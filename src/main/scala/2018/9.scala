package advent2018
import common.{Day, CircularZipper}
import scala.io.Source

class Day9(source: Source) extends Day {

  val input = source.mkString.trim
  val regex = """(\d+) players; last marble is worth (\d+) points""".r
  val (playerCount, lastScore) = input match {
    case regex(x, y) => (x.toInt, y.toInt)
  }

  def moves: Iterator[(Int, Long, CircularZipper[Int])] = {
    Iterator.iterate((0, 0L, CircularZipper(List(0)))){case (number, _, marbles) =>
      val newNumber = number + 1
      val (points, newMarbles) = if (newNumber % 23 == 0) {
        val movedLeft = marbles.moveLeftN(7)
        val removed = movedLeft.current
        (removed + newNumber, movedLeft.delete.moveRight)
      } else {
        (0, marbles.moveRight.insertRight(newNumber))
      }
      (newNumber, points, newMarbles)
    }
  }

  def maxScore(lastScore: Int): Long = {
    val playerTurn = Iterator.continually(1 to playerCount).flatten take (lastScore + 1)
    val scores = moves drop 1 map {_._2}
    val playerScores = playerTurn.zip(scores).foldLeft(Map.empty[Int, Long]){case (result, (player, score)) =>
      val previousScore = result.getOrElse(player, 0L)
      result + (player -> (score + previousScore))
    }
    playerScores.maxBy{_._2}._2
  }

  override def answer1: String = maxScore(lastScore).toString
  override def answer2: String = maxScore(lastScore * 100).toString
}
