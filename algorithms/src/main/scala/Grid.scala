package algorithms

import parse.Read
import Grid.{Pos, Group}
import scala.util.matching.Regex

given Read[Grid] with
  def read(input: String): Grid =
    Grid(input)

class Grid private (protected val cells: Map[Pos, Char]) derives CanEqual:
  lazy val minRow = cells.keys.map(_.row).min
  lazy val maxRow = cells.keys.map(_.row).max
  lazy val minCol = cells.keys.map(_.col).min
  lazy val maxCol = cells.keys.map(_.col).max

  override def toString: String =
    (minRow to maxRow).map{row =>
      (minCol to maxCol).map{col =>
        cells.getOrElse(Pos(row, col), ' ')
      }.mkString
    }.mkString("\n")

  def apply(pos: Pos): Char =
    cells(pos)

  def get(pos: Pos): Option[Char] =
    cells.get(pos)

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

  def vTree(obstacle: Char => Boolean = _ == '#')(using Grid.Neighbors): VTree[Pos] = new VTree{
    override def children(node: Pos, visited: Set[Pos]): Iterator[Pos] =
      val allNeighbors = node.neighbors.filter(cells.contains).filterNot(pos => obstacle(cells(pos)))
      (allNeighbors -- visited).iterator
  }

  def aStar(goal: Pos, obstacle: Char => Boolean = _ == '#')(using Grid.Neighbors): AStar[Pos, Int] =
    new AStar[Pos, Int](_ == goal, _.manhattan(goal), (_, _) => 1, 0, _.neighbors.filter(cells.contains).filterNot(pos => obstacle(cells(pos))))

  def allPos: Set[Pos] = cells.keySet

  def neighborCount(pos: Pos, p: Char => Boolean = _ == '#')(using Grid.Neighbors): Int =
    pos.neighbors.toList.flatMap(cells.get).count(p)

  override def equals(other: Any): Boolean =
    cells == other.asInstanceOf[Grid].cells

  override def hashCode: Int = cells.##

  def life(rule: (Char, Int) => Char)(using Grid.Neighbors): Grid =
    allPos.foldLeft(this){(accum, pos) =>
      val current = apply(pos)
      val count = neighborCount(pos)
      accum.updated(pos, rule(current, count))
    }

object Grid:
  opaque type Pos = (Int, Int)
  given CanEqual[Pos, Pos] = CanEqual.derived

  object Pos:
    def apply(row: Int, col: Int): Pos = (row, col)

  extension (p: Pos)
    def row: Int = p._1
    def col: Int = p._2
    def neighbors(using n: Neighbors): Set[Pos] = n.neighbors(p)
    def manhattan(other: Pos): Int = Math.abs(row - other.row) + Math.abs(col - other.col)
    def north: Pos = (p._1 - 1, p._2)
    def south: Pos = (p._1 + 1, p._2)
    def east:  Pos = (p._1, p._2 + 1)
    def west:  Pos = (p._1, p._2 - 1)

  def apply(string: String): Grid =
    val cells = string.linesIterator.zipWithIndex.flatMap{(line, row) =>
      line.zipWithIndex.map{(char, col) =>
        (row, col) -> char
      }
    }.toMap
    new Grid(cells)

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
