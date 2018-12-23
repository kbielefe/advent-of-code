package advent2018
import common.{Day, Grid}
import scala.io.Source
import java.util.UUID
import monix.eval.Coeval
import monix.tail.Iterant
import scala.collection.immutable.Queue

class Day15(source: Source) extends Day {
  abstract class Cell
  abstract class Creature(val attack: Int, val hitPoints: Int, val id: UUID) extends Cell {
    def setHitPoints(newHitPoints: Int): Creature
    def getAttack(extra: Int): Int
  }

  case class Wall() extends Cell
  case class Elf(override val attack:    Int = 3, override val hitPoints: Int = 200, override val id: UUID = UUID.randomUUID()) extends Creature(attack, hitPoints, id) {
    override def setHitPoints(newHitPoints: Int): Elf = copy(hitPoints = newHitPoints)
    override def getAttack(extra: Int) = attack + extra
  }
  case class Goblin(override val attack: Int = 3, override val hitPoints: Int = 200, override val id: UUID = UUID.randomUUID()) extends Creature(attack, hitPoints, id) {
    override def setHitPoints(newHitPoints: Int): Goblin = copy(hitPoints = newHitPoints)
    override def getAttack(extra: Int) = attack
  }

  def charToCell(char: Char): Cell = char match {
    case '#' => Wall()
    case 'E' => Elf()
    case 'G' => Goblin()
  }

  def cellToChar(cell: Cell): Char = cell match {
    case c: Wall   => '#'
    case c: Elf    => 'E'
    case c: Goblin => 'G'
  }

  def printGrid(grid: Grid[Cell]): Unit = {
    grid.getLines('.', cellToChar) foreach println
    println("")
  }

  lazy val grid = Grid(0, 0, '.', source, _ => None, charToCell _)

  def isEnemy(of: Creature)(potentialEnemy: Cell): Boolean = (of, potentialEnemy) match {
    case (Elf(_, _, _),    Goblin(_, _, _)) => true
    case (Goblin(_, _, _), Elf(_, _, _))    => true
    case _                            => false
  }

  def targetInRange(c: Creature, grid: Grid[Cell], x: Int, y: Int): Option[(Int, Int, Creature)] = {
    val squares = List((x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1))
    val cells = squares.map{case (x, y) => grid.getCell(x, y).map{c => (x, y, c)}}.flatten
    val enemies = cells.filter{case (_, _, e) => isEnemy(c)(e)}.map{case (x, y, c) => (x, y, c.asInstanceOf[Creature])}
    val sorted = enemies.sortBy{case (x, y, c) => (c.hitPoints, y, x)}
    sorted.headOption
  }

  def allTargets(c: Creature, grid: Grid[Cell]): List[(Int, Int)] = {
    grid.readingOrder(isEnemy(c))
  }

  def adjacentOpenSquares(grid: Grid[Cell], targets: List[(Int, Int)]): Queue[(Int, Int)] = {
    val adjacentSquares = targets.flatMap{case (x, y) => List((x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1))}
    Queue(adjacentSquares.filter{case (x, y) => !grid.getCell(x, y).isDefined}:_*)
  }

  def closestTargetSquares(x: Int, y: Int, grid: Grid[Cell], targets: Set[(Int, Int)]): Option[(List[(Int, Int)], Int, Int)] = {
    def neighbors(p: (Int, Int)): Queue[(Int, Int)] = adjacentOpenSquares(grid, List(p))
    val search = Grid.breadthFirstTraverse[Coeval, (Int, Int)]((x, y), neighbors)
    val dropNonTargets = search.dropWhile{x => !(targets contains x._3)}
    val depth: Option[Int] = dropNonTargets.map{_._2}.headOptionL.value
    if (depth.isDefined) {
      val targetsAtSameDepth = dropNonTargets
        .takeWhile{_._2 == depth.get}
        .toListL
        .value
        .sortBy{case (_, _, (tx, ty)) => (ty, tx)}
        .filter{case (_, _, (tx, ty)) => targets contains (tx, ty)}

      targetsAtSameDepth.map{case (path, _, (tx, ty)) => (path, tx, ty)}.headOption
    } else {
      None
    }
  }

  def possiblyMove(grid: Grid[Cell], attackerCell: Creature, x: Int, y: Int): (Grid[Cell], Int, Int) = {
    def neighbors(p: (Int, Int)): List[(Int, Int)] = adjacentOpenSquares(grid, List(p)).toList
    val inRange = targetInRange(attackerCell, grid, x, y)
    if (inRange.isDefined) {
      (grid, x, y)
    } else {
      val targets = allTargets(attackerCell, grid)
      val squares = adjacentOpenSquares(grid, targets)
      val closest = closestTargetSquares(x, y, grid, squares.toSet)
      closest map {case (path, cx, cy) =>
        val (moveX, moveY) = path.reverse.drop(1).head
        val movedGrid = grid.move((x, y), (moveX, moveY))
        (movedGrid, moveX, moveY)
      } getOrElse (grid, x, y)
    }
  }

  def possiblyAttack(grid: Grid[Cell], attackerCell: Creature, x: Int, y: Int, extraAttack: Int = 0): Grid[Cell] = {
    val target = targetInRange(attackerCell, grid, x, y)
    target map {case (tx, ty, t) =>
      val newHitPoints = t.hitPoints - attackerCell.getAttack(extraAttack)
      if (newHitPoints <= 0) {
        grid.delete((tx, ty))
      } else {
        grid.replace((tx, ty), t.setHitPoints(newHitPoints))
      }
    } getOrElse grid
  }

  def battleOver(grid: Grid[Cell]): Boolean = {
    !(grid.contains(_.isInstanceOf[Elf]) && grid.contains(_.isInstanceOf[Goblin]))
  }

  def turn(extraAttack: Int)(grid: Grid[Cell], player: (Int, Int, UUID)): (Grid[Cell], Boolean) = {
    val (x, y, uuid) = player
    val attacker = grid.getCell(x, y).filter(_.isInstanceOf[Creature]).map{_.asInstanceOf[Creature]}.filter(_.id == uuid)
    attacker map {a =>
      val (possiblyMovedGrid, xAfterMove, yAfterMove) = possiblyMove(grid, a, x, y)
      val possiblyAttackedGrid = possiblyAttack(possiblyMovedGrid, a, xAfterMove, yAfterMove, extraAttack)
      (possiblyAttackedGrid, battleOver(possiblyAttackedGrid))
    } getOrElse ((grid, false))
  }

  def cellOrder(grid: Grid[Cell]): List[(Int, Int, UUID)] = {
    val readingOrder: List[(Int, Int)] = grid.readingOrder(_.isInstanceOf[Creature])
    readingOrder.map{case (x, y) => (x, y, grid.getCell(x, y).get.asInstanceOf[Creature].id)}
  }

  def battleOutcome(grid: Grid[Cell], extraAttack: Int = 0): Int = {
    val turns: Iterant[Coeval, (Grid[Cell], Boolean, Int)] = grid.turns[Coeval, Boolean, (Int, Int, UUID)](turn(extraAttack), cellOrder)
    val (finalGrid, _, rounds) = turns.dropWhile(!_._2).drop(1).headOptionL.value.get
    val allCreatures = finalGrid.readingOrder(_.isInstanceOf[Creature]).map{case (x, y) => finalGrid.getCell(x, y).get.asInstanceOf[Creature]}
    val hitPoints = allCreatures.map{_.hitPoints}.sum
    rounds * hitPoints
  }
  override def answer1: String = battleOutcome(grid).toString
  override def answer2: String = ???
}
