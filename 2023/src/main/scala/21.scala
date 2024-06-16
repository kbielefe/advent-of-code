package day21

import spire.math.Rational
import algorithms.{*, given}, algorithms.spire.given
import Grid.{Neighbors, Pos}
import parse.{*, given}

extension (g: Grid)
  def isRock(pos: Pos): Boolean =
    val normalized = Pos(pos.row %+ g.height, pos.col %+ g.width)
    g(normalized) == '#'

object Puzzle extends runner.Day[Grid, Int, Rational]:
  given Neighbors = Grid.NSEWNeighbors

  def part1(grid: Grid): Int =
    answer(grid, 64)

  def answer(grid: Grid, steps: Int): Int =
    val start = grid.find('S').get
    Iterator.iterate(Set(start))(neighbors(grid)).drop(steps).next.size

  def part2(grid: Grid): Rational =
    val steps = 26501365
    val offset = steps % grid.width
    val xs = Vector(offset, offset + grid.width, offset + 2 * grid.width)
    val points = xs.map(x => Rational(x) -> Rational(answer(grid, x)))
    Lagrange(points)(steps)

  private def neighbors(grid: Grid)(gardens: Set[Pos]): Set[Pos] =
    gardens
      .flatMap(_.neighbors)
      .filterNot(grid.isRock)
