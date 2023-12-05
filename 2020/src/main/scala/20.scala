package day20

import algorithms.{Grid, given}
import parse.{*, given}

case class Edge(tile: Tile, side: String, flipped: Boolean, signature: String)

case class Tile(number: Int, grid: Grid):
  override def toString: String = s"Tile $number"

  def edges: Set[Edge] = Set(
    Edge(this, "top",    false, grid.row(grid.minRow)),
    Edge(this, "bottom", false, grid.row(grid.maxRow)),
    Edge(this, "top",     true, grid.row(grid.minRow).reverse),
    Edge(this, "bottom",  true, grid.row(grid.maxRow).reverse),
    Edge(this, "left",   false, grid.col(grid.minCol)),
    Edge(this, "right",  false, grid.col(grid.maxCol)),
    Edge(this, "left",    true, grid.col(grid.minCol).reverse),
    Edge(this, "right",   true, grid.col(grid.maxCol).reverse)
  )

type I = List[Tile ~ """(?s)Tile (\d+):\n(.+)"""] - "\n\n"

object Puzzle extends runner.Day[I, Long, Int]:
  def part1(tiles: I): Long =
    val edges = tiles.flatMap(_.edges)
    val edgesBySignature = edges.groupBy(_.signature)
    val neighbors: Map[Tile, Iterable[(Edge, Edge)]] = edgesBySignature
      .map(_._2)
      .filter(_.size == 2)
      .flatMap{case List(x, y) => List((x.tile -> (x, y)), (y.tile -> (y, x)))}
      .groupMap(_._1)(_._2)
    neighbors.filter(_._2.size == 4).map(_._1.number.toLong).product

  def part2(tiles: I): Int =
    ???
