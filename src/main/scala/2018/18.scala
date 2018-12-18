package advent2018
import common.{Day, Grid}
import scala.io.Source
import monix.tail.Iterant
import monix.eval.Coeval

class Day18(source: Source) extends Day {
  sealed abstract class Cell
  case class Lumberyard() extends Cell
  case class Trees()      extends Cell

  def under(char: Char): Option[Char] = None

  def toCell(char: Char): Cell = char match {
    case '#' => Lumberyard()
    case '|' => Trees()
  }

  def toChar(cell: Cell): Char = cell match {
    case Lumberyard() => '#'
    case Trees()      => '|'
  }

  // Returns (count of adjacent lumberyards, count of adjacent trees)
  def neighborCounts(grid: Grid[Cell], x: Int, y: Int): (Int, Int) = {
    val neighbors = for {
      nX <- (x - 1) to (x + 1)
      nY <- (y - 1) to (y + 1)
    } yield if (x != nX || y != nY) grid.getCell(nX, nY) else None
    val lumberyardCount = neighbors.flatten.count(_.isInstanceOf[Lumberyard])
    val treesCount      = neighbors.flatten.count(_.isInstanceOf[Trees])
    (lumberyardCount, treesCount)
  }

  def move(grid: Grid[Cell], width: Int = 50, height: Int = 50): Grid[Cell] = {
    val allCoords = for {
      x <- 0 until width
      y <- 0 until height
    } yield (x, y)

    val newCells = for {
      (x, y) <- allCoords
      (lumberyardCount, treesCount) = neighborCounts(grid, x, y)
      cell = grid.getCell(x, y)
    } yield cell match {
      case None               => if (treesCount >= 3)                         Some(Trees())      else None
      case Some(Trees())      => if (lumberyardCount >= 3)                    Some(Lumberyard()) else Some(Trees())
      case Some(Lumberyard()) => if (lumberyardCount >= 1 && treesCount >= 1) Some(Lumberyard()) else None
    }

    allCoords.zip(newCells).foldLeft(grid){case (grid, (coords, newCell)) =>
      newCell map {grid.replace(coords, _)} getOrElse {grid.delete(coords)}
    }
  }

  def moves(grid: Grid[Cell], width: Int = 50, height: Int = 50): Iterant[Coeval, Grid[Cell]] = {
    Iterant.pure(grid) ++ moves(move(grid, width, height), width, height)
  }

  lazy val grid = Grid(0, 0, '.', source, under _, toCell _)

  def totalValue(grid: Grid[Cell], width: Int = 50, height: Int = 50): Int = {
    val resources = for {
      x <- 0 until width
      y <- 0 until height
    } yield grid.getCell(x, y)
    val lumberyardCount = resources.flatten.count(_.isInstanceOf[Lumberyard])
    val treesCount      = resources.flatten.count(_.isInstanceOf[Trees])
    lumberyardCount * treesCount
  }

  override def answer1: String = totalValue(moves(grid).drop(10).headOptionL.value.get).toString
  override def answer2: String = ???
}
