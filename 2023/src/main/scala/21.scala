package day21
import algorithms.{*, given}
import Grid.{Neighbors, Pos}
import parse.{*, given}

extension (g: Grid)
  def isRock(pos: Pos): Boolean =
    val normalized = Pos(pos.row %+ g.height, pos.col %+ g.width)
    g(normalized) == '#'

object Puzzle extends runner.Day[Grid, Long, BigInt]:
  given Neighbors = Grid.NSEWNeighbors

  def part1(grid: Grid): Long =
    answer(grid, 64)

  def answer(grid: Grid, steps: Int): Long =
    val start = grid.find('S').get
    Iterator.iterate(Set(start))(neighbors(grid)).drop(steps).next.size

  def part2(grid: Grid): BigInt =
    val steps = 26501365
    val offset = steps % grid.width
    val target = (steps - offset) / grid.width
    val xs = Vector(offset, offset + grid.width, offset + 2 * grid.width)
    val points = xs.map(x => BigInt(x) -> BigInt(answer(grid, x)))
    Lagrange(points)(target)

  private def neighbors(grid: Grid)(gardens: Set[Pos]): Set[Pos] =
    gardens
      .flatMap(_.neighbors)
      .filterNot(grid.isRock)
