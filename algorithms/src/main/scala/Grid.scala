package algorithms

import parse.Read
import Grid.Pos

given Read[Grid] with
  def read(input: String): Grid =
    Grid(input)

class Grid private (cells: Map[Pos, Char]):
  override def toString: String =
    val minRow = cells.keys.map(_.row).min
    val maxRow = cells.keys.map(_.row).max
    val minCol = cells.keys.map(_.col).min
    val maxCol = cells.keys.map(_.col).max
    (minRow to maxRow).map{row =>
      (minCol to maxCol).map{col =>
        cells.getOrElse(Pos(row, col), ' ')
      }.mkString
    }.mkString("\n")

  def apply(pos: Pos): Char =
    cells(pos)

  def charSet: Set[Char] =
    cells.valuesIterator.toSet

  def find(char: Char): Option[Pos] =
    cells.find(_._2 == char).map(_._1)

  def vTree(obstacle: Char => Boolean = _ == '#'): VTree[Pos] = new VTree{
    override def children(node: Pos, visited: Set[Pos]): Iterator[Pos] =
      val allNeighbors = node.neighbors.filter(cells.contains).filterNot(pos => obstacle(cells(pos)))
      (allNeighbors -- visited).iterator
  }

  def aStar(goal: Pos, obstacle: Char => Boolean = _ == '#'): AStar[Pos, Int] =
    new AStar[Pos, Int](_ == goal, _.manhattan(goal), (_, _) => 1, 0, _.neighbors.filter(cells.contains).filterNot(pos => obstacle(cells(pos))))

object Grid:
  opaque type Pos = (Int, Int)
  given CanEqual[Pos, Pos] = CanEqual.derived

  object Pos:
    def apply(row: Int, col: Int): Pos = (row, col)

  extension (p: Pos)
    def row: Int = p._1
    def col: Int = p._2
    def neighbors: Set[Pos] =
      Set((row + 1, col), (row -1 , col), (row, col + 1), (row, col - 1))
    def manhattan(other: Pos): Int = Math.abs(row - other.row) + Math.abs(col - other.col)

  def apply(string: String): Grid =
    val cells = string.linesIterator.zipWithIndex.flatMap{(line, row) =>
      line.zipWithIndex.map{(char, col) =>
        (row, col) -> char
      }
    }.toMap
    new Grid(cells)
