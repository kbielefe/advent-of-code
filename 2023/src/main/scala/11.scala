package day11
import parse.{*, given}
import algorithms.{Grid, given}
import Grid.Pos

object Puzzle extends runner.Day[Grid, Long, Long]:
  def part1(grid: Grid): Long =
    val emptyRows = (grid.minRow to grid.maxRow).filter(row => grid.row(row).forall(_ == '.')).toSet
    val emptyCols = (grid.minCol to grid.maxCol).filter(col => grid.col(col).forall(_ == '.')).toSet
    val galaxies = grid.allPos.filter(pos => grid(pos) == '#').toList
    galaxies.combinations(2).map(distance(emptyRows, emptyCols, 1)).sum

  def part2(grid: Grid): Long =
    val emptyRows = (grid.minRow to grid.maxRow).filter(row => grid.row(row).forall(_ == '.')).toSet
    val emptyCols = (grid.minCol to grid.maxCol).filter(col => grid.col(col).forall(_ == '.')).toSet
    val galaxies = grid.allPos.filter(pos => grid(pos) == '#').toList
    galaxies.combinations(2).map(distance(emptyRows, emptyCols, 999999)).sum

  def distance(emptyRows: Set[Int], emptyCols: Set[Int], expansion: Long)(pair: Seq[Pos]): Long =
    val Seq(x, y) = pair: @unchecked
    val rowDiff = (x.row - y.row).abs + expansion * emptyRows.count(row => (row > x.row && row < y.row) || (row > y.row && row < x.row))
    val colDiff = (x.col - y.col).abs + expansion * emptyCols.count(col => (col > x.col && col < y.col) || (col > y.col && col < x.col))
    rowDiff + colDiff
