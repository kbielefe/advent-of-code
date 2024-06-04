package day20

import algorithms.{Grid, Matrix, given}
import algorithms.Grid.Pos
import algorithms.Grid.Neighbors
import parse.{*, given}
import scala.annotation.tailrec

case class Tile(number: Int, grid: Grid):
  override def toString: String = s"Tile $number"

  def removeEdges: Grid =
    grid
      .removeRow(grid.minRow)
      .removeRow(grid.maxRow)
      .removeCol(grid.minCol)
      .removeCol(grid.maxCol)

  def edges: Set[String] = Set(
    grid.row(grid.minRow),
    grid.row(grid.maxRow),
    grid.col(grid.minCol),
    grid.col(grid.maxCol),
    grid.row(grid.minRow).reverse,
    grid.row(grid.maxRow).reverse,
    grid.col(grid.minCol).reverse,
    grid.col(grid.maxCol).reverse
  )

  def rightEdge: String =
    grid.col(grid.maxCol)

  def leftEdge: String =
    grid.col(grid.minCol)

  def bottomEdge: String =
    grid.row(grid.maxRow)

  def topEdge: String =
    grid.row(grid.minRow)

  def commonEdgesWith(other: Tile): Set[String] =
    edges & other.edges

  @tailrec
  final def transformUntil(t: Matrix[3, 3, Int], p: Tile => Boolean): Tile =
    if p(this) then
      this
    else
      Tile(number, grid.transform(t)).transformUntil(t, p)

given Read[Tile] = Read("""(?s)Tile (\d+):\n(.+)""".r)

type I = List[Tile]
given Read[I] = Read("\n\n")

val monster = Grid(
  """                  #
    |#    ##    ##    ###
    | #  #  #  #  #  #   """.stripMargin).filter(_ == '#')

object Puzzle extends runner.Day[I, Long, Int]:
  def part1(tiles: I): Long =
    getNeighbors(tiles)
      .filter(_._2.size == 2)
      .map(_._1.number.toLong)
      .product

  def part2(tiles: I): Int =
    val neighbors = getNeighbors(tiles)
    val corner = neighbors.find(_._2.size == 2).get._1
    val edgePiece = neighbors(corner).head
    val initialPlaced = Map(
      Pos(0, 0) -> corner,
      Pos(0, 1) -> edgePiece
    )
    val possibilities = Map(
      Pos(1, 0) -> (neighbors(corner) - edgePiece),
      Pos(1, 1) -> (neighbors(edgePiece) - corner),
      Pos(0, 2) -> (neighbors(edgePiece) - corner)
    )
    val placed = placeTiles(neighbors, possibilities, initialPlaced)
    val commonRight  = corner.commonEdgesWith(placed(Pos(0, 1)))
    val commonBottom = corner.commonEdgesWith(placed(Pos(1, 0)))
    val transformedCorner =
      corner
        .transformUntil(Matrix.rotateCW, tile => commonRight.contains(tile.rightEdge))
        .transformUntil(Matrix.reflectX, tile => commonBottom.contains(tile.bottomEdge))
    val transformed = transformTiles(placed, Map(Pos(0, 0) -> transformedCorner), Pos(0, 1))
    val assembled = assemble(transformed)
    val monstersRemoved = findMonsters(assembled).foldLeft(assembled)((grid: Grid, monsterPos: Pos) => grid -- monster.transform(Matrix.translate(monsterPos.col, monsterPos.row)))
    monstersRemoved.count(_ == '#')

  given Neighbors = Grid.NSEWNeighbors

  def findMonsters(grid: Grid): List[Pos] =
    (grid.minRow to grid.maxRow).toList.flatMap{row =>
      (grid.minCol to grid.maxCol).flatMap{col =>
        val transformed = monster.transform(Matrix.translate(col, row))
        Option.when((transformed & grid) == transformed)(Pos(row, col))
      }
    }

  def assemble(tiles: Map[Pos, Tile]): Grid =
    tiles.foldLeft(Grid.empty){case (grid, (pos, tile)) =>
      val tileGrid = tile.removeEdges
      val rowOffset = pos.row * 8 - tileGrid.minRow
      val colOffset = pos.col * 8 - tileGrid.minCol
      val transformed = tileGrid.transform(Matrix.translate(colOffset, rowOffset))
      grid ++ transformed
    }

  @tailrec
  def transformTiles(placed: Map[Pos, Tile], transformed: Map[Pos, Tile], pos: Pos): Map[Pos, Tile] =
    if transformed.size == placed.size then
      transformed
    else
      if pos.row == 0 then
        val goal = transformed(pos.west).rightEdge
        val transformedTile = placed(pos)
          .transformUntil(Matrix.rotateCW, tile => tile.leftEdge == goal || tile.leftEdge == goal.reverse)
          .transformUntil(Matrix.reflectX, tile => tile.leftEdge == goal)
        val newTransformed = transformed + (pos -> transformedTile)
        val newPos = if pos.col == 11 then Pos(pos.row + 1, 0) else pos.east
        transformTiles(placed, newTransformed, newPos)
      else
        val goal = transformed(pos.north).bottomEdge
        val transformedTile = placed(pos)
          .transformUntil(Matrix.rotateCW, tile => tile.topEdge == goal || tile.topEdge == goal.reverse)
          .transformUntil(Matrix.reflectY, tile => tile.topEdge == goal)
        val newTransformed = transformed + (pos -> transformedTile)
        val newPos = if pos.col == 11 then Pos(pos.row + 1, 0) else pos.east
        transformTiles(placed, newTransformed, newPos)

  @tailrec
  def placeTiles(neighbors: Map[Tile, Set[Tile]], possibilities: Map[Pos, Set[Tile]], placed: Map[Pos, Tile]): Map[Pos, Tile] =
    if possibilities.isEmpty then
      placed
    else
      val (pos, tile) = possibilities.filter(_._2.size == 1).view.mapValues(_.head).head
      val newPlaced = placed + (pos -> tile)
      val withoutTile = possibilities.map((pos, tiles) => (pos, tiles - tile)).filterNot(_._2.isEmpty)
      val unplacedTiles = neighbors(tile) -- placed.values
      val unplacedNeighbors = (pos.neighbors -- placed.keySet).filter(pos => pos.row >= 0 && pos.row <= 11 && pos.col >= 0 && pos.col <= 11)
      val newPossibilities = unplacedNeighbors.foldLeft(withoutTile){(possibilities, pos) =>
        val newTiles = possibilities.get(pos) match
          case Some(tiles) => tiles & unplacedTiles
          case None        => unplacedTiles
        possibilities.updated(pos, newTiles)
      }
      placeTiles(neighbors, newPossibilities, newPlaced)

  def getNeighbors(tiles: I): Map[Tile, Set[Tile]] =
    tiles
      .flatMap(tile => tile.edges.map(edge => (tile, edge)))
      .groupMap(_._2)(_._1)
      .values
      .collect{case List(x, y) => List((x, y), (y, x))}
      .flatten
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
