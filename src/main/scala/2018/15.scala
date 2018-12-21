package advent2018
import common.{Day, Grid}
import scala.io.Source
import java.util.UUID
import monix.eval.Coeval
import monix.tail.Iterant

class Day15(source: Source) extends Day {
  abstract class Cell
  abstract class Creature(val attack: Int, val hitPoints: Int, val id: UUID = UUID.randomUUID()) extends Cell

  case class Wall() extends Cell
  case class Elf(override val attack:    Int = 3, override val hitPoints: Int = 200) extends Creature(attack, hitPoints)
  case class Goblin(override val attack: Int = 3, override val hitPoints: Int = 200) extends Creature(attack, hitPoints)

  def charToCell(char: Char): Cell = char match {
    case '#' => Wall()
    case 'E' => Elf()
    case 'G' => Goblin()
  }

  lazy val grid = Grid(0, 0, '.', source, _ => None, charToCell _)

  def isEnemy(of: Creature, potentialEnemy: Cell): Boolean = (of, potentialEnemy) match {
    case (Elf(_, _),    Goblin(_, _)) => true
    case (Goblin(_, _), Elf(_, _))    => true
    case _                            => false
  }

  def targetInRange(c: Creature, grid: Grid[Cell], x: Int, y: Int): Option[(Int, Int, Creature)] = {
    val squares = List((x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1))
    val cells = squares.map{case (x, y) => grid.getCell(x, y).map{c => (x, y, c)}}.flatten
    cells.find{case (_, _, e) => isEnemy(c, e)}.map{case (x, y, c) => (x, y, c.asInstanceOf[Creature])}
  }

  def turn(grid: Grid[Cell], player: (Int, Int, UUID)): (Grid[Cell], Boolean) = {
    val attacker = grid.getCell(player._1, player._2).filter(_.isInstanceOf[Creature]).filter(_.asInstanceOf[Creature].id == player._3)
    attacker map {a =>
      /*
       * if not in range of a target
       *   Identify all possible targets
       *   Identify open squares adjacent to targets
       *   move
       *     get closest target squares
       *     choose first in reading order
       *     take one step toward target
       * if in range of a target
       *   attack
       * if no enemies left
       *   return false
       */
      (grid, true)
    } getOrElse ((grid, true))
  }

  def cellOrder(grid: Grid[Cell]): List[(Int, Int, UUID)] = {
    val readingOrder: List[(Int, Int)] = grid.readingOrder(_.isInstanceOf[Creature])
    readingOrder.map{case (x, y) => (x, y, grid.getCell(x, y).get.asInstanceOf[Creature].id)}
  }

  def battleOutcome: Int = {
    val turns: Iterant[Coeval, (Grid[Cell], Boolean, Int)] = grid.turns[Coeval, Boolean, (Int, Int, UUID)](turn, cellOrder)
    val (finalGrid, _, rounds) = turns.dropWhile(_._2).headOptionL.value.get
    val hitPoints = finalGrid.readingOrder(_.isInstanceOf[Creature]).map(_.asInstanceOf[Creature].hitPoints).sum
    rounds * hitPoints
  }
  override def answer1: String = grid.getLines().mkString("\n")
  override def answer2: String = ???
}
