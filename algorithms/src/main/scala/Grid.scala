package algorithms

import _root_.breeze.linalg.*
import io.circe.*
import parse.Read
import Grid.{Pos, Group}
import scala.util.matching.Regex

given Read[Grid] with
  def read(input: String): Grid =
    Grid(input)

class Grid private (val cells: Map[Pos, Char]) derives CanEqual:
  lazy val minRow = cells.keys.map(_.row).min
  lazy val maxRow = cells.keys.map(_.row).max
  lazy val minCol = cells.keys.map(_.col).min
  lazy val maxCol = cells.keys.map(_.col).max
  lazy val width = maxCol - minCol + 1
  lazy val height = maxRow - minRow + 1

  override def toString: String =
    (minRow to maxRow).map{row =>
      (minCol to maxCol).map{col =>
        cells.getOrElse(Pos(row, col), ' ')
      }.mkString
    }.mkString("\n")

  // Need to reflect around Y axis to get normal positive-Y is up transformations
  def transform(m: Matrix[Int]): Grid =
    new Grid(cells.map((pos, char) => (Pos.fromAffine(m * pos.toAffine), char)))

  def map(f: Char => Char): Grid =
    new Grid(cells.view.mapValues(f).toMap)

  def apply(pos: Pos): Char =
    cells(pos)

  def get(pos: Pos): Option[Char] =
    cells.get(pos)

  def getOrElse(pos: Pos, orElse: Char): Char =
    cells.getOrElse(pos, orElse)

  def updated(p: Pos, c: Char): Grid =
    new Grid(cells.updated(p, c))

  def charSet: Set[Char] =
    cells.valuesIterator.toSet

  def contains(pos: Pos): Boolean =
    cells.contains(pos)

  def count(p: Char => Boolean): Int =
    cells.valuesIterator.count(p)

  def find(char: Char): Option[Pos] =
    cells.find(_._2 == char).map(_._1)

  def row(row: Int): String =
    (minCol to maxCol).map(col => cells.getOrElse(Pos(row, col), ' ')).mkString

  def rows: Iterator[String] =
    (minRow to maxRow).iterator.map(row)

  def col(col: Int): String =
    cells.filter(_._1.col == col).toList.sortBy(_._1.row).map(_._2).mkString

  def cols: Iterator[String] =
    (minCol to maxCol).iterator.map(col)

  def removeRow(row: Int): Grid =
    new Grid(cells.filterNot(_._1.row == row))

  def removeCol(col: Int): Grid =
    new Grid(cells.filterNot(_._1.col == col))

  def ++(other: Grid): Grid =
    new Grid(cells ++ other.cells)

  def --(other: Grid): Grid =
    new Grid(cells -- other.cells.keySet)

  def --(xs: Set[Pos]): Grid =
    new Grid(cells -- xs)

  def +(cell: (Pos, Char)): Grid =
    new Grid(cells + cell)

  def keepOnlyPositionsIn(positions: Set[Pos]): Grid =
    new Grid(cells.filter((pos, _) => positions.contains(pos)))

  // Returns a new Grid with only the overlapping cells that match
  def &(other: Grid): Grid =
    val commonPos = cells.keySet & other.cells.keySet
    new Grid(commonPos.filter(pos => cells(pos) == other.cells(pos)).map(pos => (pos, cells(pos))).toMap)

  // Returns if the overlapping positions of each grid are equal
  def overlapEquals(other: Grid): Boolean =
    val commonPos = cells.keySet & other.cells.keySet
    commonPos.forall(pos => cells(pos) == other.cells(pos))

  def filter(p: Char => Boolean): Grid =
    new Grid(cells.filter((pos, char) => p(char)))

  def findAll(length: Int, regex: Regex): Set[Pos] =
    cells.keySet.flatMap{pos =>
      val sequence = (pos.col until (pos.col + length)).map(col => cells.get(Pos(pos.row, col)))
      if sequence.forall(_.isDefined) then
        val string = sequence.flatten.mkString
        Option.when(regex.matches(string))(pos)
      else
        None
    }

  def findAllVertical(length: Int, regex: Regex): Set[Pos] =
    cells.keySet.flatMap{pos =>
      val sequence = (pos.row until (pos.row + length)).map(row => cells.get(Pos(row, pos.col)))
      if sequence.forall(_.isDefined) then
        val string = sequence.flatten.mkString
        Option.when(regex.matches(string))(pos)
      else
        None
    }

  /** Find consecutive characters that satisfy the predicate.  */
  def findGroups(p: Char => Boolean): List[Group] =
    cells.filter((pos, char) => p(char) && (!cells.contains(pos.west) || !p(apply(pos.west))))
      .map(_._1)
      .map{pos =>
        val lastPos = Iterator.iterate(pos)(_.east).takeWhile(cells.contains).takeWhile(pos => p(apply(pos))).toList.last
        val length = lastPos.col - pos.col + 1
        val string = (pos.col until (pos.col + length)).map(col => apply(Pos(pos.row, col))).mkString
        new Group(pos, length, string)
      }.toList

  def vTree(obstacle: Char => Boolean = _ == '#')(using Grid.Neighbors): VTree[Pos] = new VTree:
    override def children(node: Pos, visited: Set[Pos]): Iterator[Pos] =
      val allNeighbors = node.neighbors.filter(cells.contains).filterNot(pos => obstacle(cells(pos)))
      (allNeighbors -- visited).iterator

  def aStar(goal: Pos, obstacle: Char => Boolean = _ == '#')(using Grid.Neighbors): AStar[Pos, Int] =
    new AStar[Pos, Int](_ == goal, _.manhattan(goal), (_, _) => 1, 0, _.neighbors.filter(cells.contains).filterNot(pos => obstacle(cells(pos))))

  def allPos: Set[Pos] = cells.keySet

  def neighborCount(pos: Pos, p: Char => Boolean = _ == '#')(using Grid.Neighbors): Int =
    pos.neighbors.toList.flatMap(cells.get).count(p)

  override def equals(other: Any): Boolean =
    cells == other.asInstanceOf[Grid].cells

  override def hashCode: Int = cells.##

object Grid:
  opaque type Pos = (Int, Int)
  given CanEqual[Pos, Pos] = CanEqual.derived

  given KeyEncoder[Pos] with
    def apply(pos: Pos): String =
      pos.toString

  given Encoder[Grid] =
    summon[Encoder[Map[Pos, Char]]].contramap(_.cells)

  object Pos:
    def apply(row: Int, col: Int): Pos = (row, col)
    def unapply(pos: Pos): Option[(Int, Int)] = Some(pos.row, pos.col)
    def fromAffine(v: Vector[Int]): Pos =
      Pos(v(1), v(0))

  extension (p: Pos)
    def row: Int = p._1
    def col: Int = p._2
    def neighbors(using n: Neighbors): Set[Pos] = n.neighbors(p)
    def manhattan(other: Pos): Int = Math.abs(row - other.row) + Math.abs(col - other.col)
    def north: Pos = (p._1 - 1, p._2)
    def south: Pos = (p._1 + 1, p._2)
    def east:  Pos = (p._1, p._2 + 1)
    def west:  Pos = (p._1, p._2 - 1)
    def toAffine: Vector[Int] =
      DenseVector(p.col, p.row, 1)

  def apply(string: String): Grid =
    val cells = string.linesIterator.zipWithIndex.flatMap{(line, row) =>
      line.zipWithIndex.map{(char, col) =>
        (row, col) -> char
      }
    }.toMap
    new Grid(cells)

  def apply(cells: Map[Pos, Char]): Grid =
    new Grid(cells)

  def empty: Grid = new Grid(Map.empty)

  class Group(val firstPos: Pos, val length: Int, val string: String):
    def allPos: Set[Pos] =
      (firstPos.col until (firstPos.col + length)).toSet.map(col => Pos(firstPos.row, col))

    def neighbors(using n: Neighbors): Set[Pos] =
      allPos.flatMap(n.neighbors) -- allPos

  trait Neighbors:
    def neighbors(p: Pos): Set[Pos]

  object NSEWNeighbors extends Neighbors:
    def neighbors(p: Pos): Set[Pos] =
      Set(p.north, p.south, p.east, p.west)

  object DiagonalNeighbors extends Neighbors:
    def neighbors(p: Pos): Set[Pos] =
      Set(p.north, p.south, p.east, p.west, p.north.west, p.north.east, p.south.west, p.south.east)
