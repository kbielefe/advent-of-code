package day20

import algorithms.{Grid, given}
import algorithms.Grid.Pos
import algorithms.Grid.Neighbors
import parse.{*, given}
import scala.annotation.tailrec

case class Tile(number: Int, grid: Grid):
  override def toString: String = s"Tile $number"

  def edges: Set[(Tile, String)] = Set(
    grid.row(grid.minRow),
    grid.row(grid.maxRow),
    grid.row(grid.minRow).reverse,
    grid.row(grid.maxRow).reverse,
    grid.col(grid.minCol),
    grid.col(grid.maxCol),
    grid.col(grid.minCol).reverse,
    grid.col(grid.maxCol).reverse
  ).map((this, _))

given Read[Tile] = Read("""(?s)Tile (\d+):\n(.+)""".r)

type I = List[Tile] - "\n\n"

object Puzzle extends runner.Day[I, Long, Int]:
  def part1(tiles: I): Long =
    getNeighbors(tiles)
      .filter(_._2.size == 2)
      .map(_._1.number.toLong)
      .product

  def part2(tiles: I): Int =
    val neighbors = getNeighbors(tiles)
    val corner = neighbors.find(_._2.size == 2).get._1
    val edge = neighbors(corner).head
    val initialPlaced = Map(
      Pos(0, 0) -> corner,
      Pos(0, 1) -> edge
    )
    val possibilities = Map(
      Pos(1, 0) -> (neighbors(corner) - edge),
      Pos(1, 1) -> (neighbors(edge) - corner),
      Pos(0, 2) -> (neighbors(edge) - corner)
    )
    val placed = placeTiles(neighbors, possibilities, initialPlaced)
    placed.toList.sortBy((pos, _) => (pos.row, pos.col)).foreach(println)
    ???

  given Neighbors = Grid.NSEWNeighbors

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
      .flatMap(_.edges)
      .groupMap(_._2)(_._1)
      .values
      .collect{case List(x, y) => List((x, y), (y, x))}
      .flatten
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
