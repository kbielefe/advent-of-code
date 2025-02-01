package advent2018
import common.Day
import common.Numeric._
import scala.io.Source
import monix.tail.Iterant
import monix.eval.Coeval

class Day14(source: Source) extends Day {
  val input = source.mkString.trim

  case class Scoreboard(size: Int = 0, recipeScores: Vector[Int] = Vector.empty) {
    def getScore(recipe: Int): Int = if (recipe < 0) -1 else recipeScores(recipe)

    def append(score: Int): Scoreboard = Scoreboard(size + 1, recipeScores :+ score)

    def moveRight(from: Int): Int = (from + getScore(from) + 1) % size

    def patternPosition(pattern: String): Option[Int] = {
      val patternSize = pattern.size
      def existsAtOffset(offset: Int) = 
        pattern.iterator.zipWithIndex.forall{case (p, i) => p.asDigit == getScore(size - patternSize + i - offset)}

      if (existsAtOffset(0)) {
        Some(size - pattern.size)
      } else if (existsAtOffset(1)) {
        Some(size - pattern.size - 1)
      } else {
        None
      }
    }

    override def toString: String = recipeScores.mkString("Scoreboard(", ", ", ")")
  }

  object Scoreboard {
    def apply(elems: Int*): Scoreboard = {
      val vector = elems.toVector
      Scoreboard(vector.size, vector)
    }
  }

  def combineRecipes(x: Int, y: Int): Seq[Int] = (x + y).digits

  def rounds: Iterant[Coeval, Scoreboard] = {
    val initialScoreboard = Scoreboard().append(3).append(7)
    val initialState = (initialScoreboard, 0, 1)
    def nextState(prevState: (Scoreboard, Int, Int)) = {
      val (scoreboard, elf1Pos, elf2Pos) = prevState
      val newRecipes = combineRecipes(scoreboard.getScore(elf1Pos), scoreboard.getScore(elf2Pos))
      val newScoreboard = newRecipes.foldLeft(scoreboard){_ append _}
      val newElf1Pos = newScoreboard.moveRight(elf1Pos)
      val newElf2Pos = newScoreboard.moveRight(elf2Pos)
      val newState = (newScoreboard, newElf1Pos, newElf2Pos)
      (newScoreboard, newState)
    }
    Iterant.fromStateAction[Coeval, (Scoreboard, Int, Int), Scoreboard](nextState)(initialState)
  }

  def score10After(recipeCount: Int): String = {
    val finalScoreboard = rounds.dropWhile{_.size < (recipeCount + 10)}.headOptionL.apply().get
    ((recipeCount) to (recipeCount + 9)).map{finalScoreboard.getScore(_)}.mkString
  }

  def patternPosition(pattern: String): Int = {
    rounds.map{_.patternPosition(pattern)}.dropWhile{!_.isDefined}.headOptionL.apply().get.get
  }

  override def answer1: String = score10After(input.toInt)
  override def answer2: String = patternPosition(input).toString
}
