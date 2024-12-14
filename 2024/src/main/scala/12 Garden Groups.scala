package day12
import algorithms.{Grid, given}, Grid.*
import parse.{*, given}
import scala.annotation.tailrec

case class Region(char: Char, plots: Set[Pos]):
  def cost: Int = plots.size * perimeter.size

  def bulk: Int = plots.size * sides

  def perimeter: List[(Pos, Pos)] =
    plots
      .toList
      .flatMap(boundaries)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toList
      .filter(_._2 == 1)
      .map(_._1)

  def boundaries(pos: Pos): Set[(Pos, Pos)] =
    Set((pos.north, pos), (pos.west, pos), (pos, pos.east), (pos, pos.south))

  def sides: Int =
    val horizontal = perimeter.filter(_.col == _.col).map(_._1).groupMap(pos => (pos.row, plots.contains(pos)))(_.col)
    val vertical = perimeter.filter(_.row == _.row).map(_._1).groupMap(pos => (pos.col, plots.contains(pos)))(_.row)
    vertical.values.map(groupCount).sum + horizontal.values.map(groupCount).sum

  def groupCount(xs: Seq[Int]): Int =
    1 + xs.sorted.sliding(2).count:
      case Seq(l, r) => r - l > 1
      case Seq(only) => false

object Puzzle extends runner.Day[Grid, Int, Int]:
  given Neighbors = NSEWNeighbors

  def part1(grid: Grid): Int =
    getRegions(grid).map(_.cost).sum

  def part2(grid: Grid): Int =
    getRegions(grid).map(_.bulk).sum

  @tailrec
  def getRegions(grid: Grid, accum: List[Region] = List.empty): List[Region] =
    grid.headOption match
      case Some((pos, char)) =>
        val plots = grid.floodFill(pos, _ == char)
        getRegions(grid -- plots, Region(char, plots) :: accum)
      case None => accum
