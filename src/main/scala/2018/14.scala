package advent2018
import common.Day
import common.Numeric._
import scala.io.Source
import monix.tail.Iterant
import monix.eval.Coeval

class Day14(source: Source) extends Day {
  val input = source.mkString.trim.toInt

  case class Scoreboard(size: Int = 0, recipeScores: Map[Int, Int] = Map.empty[Int, Int]) {
    def getScore(recipe: Int): Int = recipeScores(recipe)
    def append(score: Int): Scoreboard = Scoreboard(size + 1, recipeScores + (size -> score))
    def moveRight(from: Int): Int = (from + getScore(from) + 1) % size
  }

  def combineRecipes(x: Int, y: Int): Seq[Int] = (x + y).digits

  def rounds: Iterant[Coeval, (Scoreboard, Int, Int)] = {
    val initialScoreboard = Scoreboard().append(3).append(7)
    val initialState = (initialScoreboard, 0, 1)
    def nextState(prevState: (Scoreboard, Int, Int)) = {
      val (scoreboard, elf1Pos, elf2Pos) = prevState
      val newRecipes = combineRecipes(scoreboard.getScore(elf1Pos), scoreboard.getScore(elf2Pos))
      val newScoreboard = newRecipes.foldLeft(scoreboard){_.append(_)}
      val newElf1Pos = newScoreboard.moveRight(elf1Pos)
      val newElf2Pos = newScoreboard.moveRight(elf2Pos)
      val newState = (newScoreboard, newElf1Pos, newElf2Pos)
      (newState, newState)
    }
    Iterant.fromStateAction[Coeval, (Scoreboard, Int, Int), (Scoreboard, Int, Int)](nextState)(initialState)
  }

  def score10After(recipeCount: Int): String = {
    val finalScoreboard = rounds.map{_._1}.dropWhile{_.size < (recipeCount + 10)}.headOptionL.apply().get
    ((recipeCount) to (recipeCount + 9)).map{finalScoreboard.getScore(_)}.mkString
  }

  override def answer1: String = score10After(input)
  override def answer2: String = ???
}
