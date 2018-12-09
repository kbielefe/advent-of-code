package advent2018
import common.Day
import scala.io.Source

// g maps node -> (back, forward)
case class LinkedCircular[A](protected val g: Map[A, (A, A)]) {

  def move(from: A, count: Int): A = {
    if (count == 0) {
      from
    } else if (count < 0) {
      Iterator.iterate(from){g(_)._1}.drop(-1 * count).next
    } else {
      Iterator.iterate(from){g(_)._2}.drop(count).next
    }
  }

  def insertAfter(pred: A, inserted: A): LinkedCircular[A] = {
    val (predBack, succ) = g(pred)
    val (_, succForward) = g(succ)
    if (pred == succ)
      LinkedCircular(g + (pred -> (inserted, inserted)) + (inserted -> (pred, succ)))
    else
      LinkedCircular(g + (pred -> (predBack, inserted)) + (inserted -> (pred, succ)) + (succ -> (inserted, succForward)))
  }

  def delete(deleted: A): LinkedCircular[A] = {
    val (pred, succ) = g(deleted)
    val (predBack, _) = g(pred)
    val (_, succForward) = g(succ)
    if (pred == succ)
      LinkedCircular(g - deleted + (pred -> (pred, succ)))
    else
      LinkedCircular(g - deleted + (pred -> (predBack, succ)) + (succ -> (pred, succForward)))
  }

  def toList(startNode: A): List[A] = {
    Iterator.iterate(startNode){g(_)._2}.take(g.size).toList
  }
}

class Day9(source: Source) extends Day {

  val input = source.mkString.trim
  val regex = """(\d+) players; last marble is worth (\d+) points""".r
  val (playerCount, lastScore) = input match {
    case regex(x, y) => (x.toInt, y.toInt)
  }

  def moves: Iterator[(Int, Int, Int, LinkedCircular[Int])] = {
    Iterator.iterate((0, 0, 0, LinkedCircular(Map(0 -> (0, 0))))){case (current, number, _, marbles) =>
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

  override def answer1: String = {
    val playerTurn = Iterator.continually(1 to playerCount).flatten take (lastScore + 1)
    val scores = moves drop 1 map {_._3}
    val playerScores = playerTurn.zip(scores).toList.groupBy(_._1).mapValues{_.map{_._2}.sum}
    val maxScore = playerScores.maxBy{_._2}._2
    maxScore.toString
  }

  override def answer2: String = ???
}
