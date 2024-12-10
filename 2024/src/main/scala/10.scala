package day10
import algorithms.{Grid, given}, Grid.{Neighbors, Pos}

object Puzzle extends runner.Day[Grid, Int, Int]:
  class AscendingNeighbors(grid: Grid) extends Neighbors:
    def neighbors(p: Pos): Set[Pos] =
      Set(p.north, p.south, p.east, p.west)
        .filter(n => grid.getOrElse(n, '0').asDigit - grid(p).asDigit == 1)

  def part1(grid: Grid): Int =
    given Neighbors = AscendingNeighbors(grid)
    grid.findAll('0').map: start =>
        grid.vTree().allReachable(start, grid(_) == '9').size
      .sum

  def part2(grid: Grid): Int =
    given Neighbors = AscendingNeighbors(grid)
    grid.findAll('0').map: start =>
        grid.vTree().pathCount(start, grid(_) == '9')
      .sum
