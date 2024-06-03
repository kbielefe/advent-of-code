package day23
import algorithms.{Grid, given}
import algorithms.Grid.{*, given}
import parse.{*, given}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    answer(grid)

  def part2(grid: Grid): Int =
    val noSlopes = grid.map{
      case '>' | '<' | '^' | 'v' => '.'
      case c => c
    }
    answer(noSlopes)

  private def answer(grid: Grid): Int =
    val startCol = grid.row(grid.minRow).indexWhere(_ == '.')
    val goalCol  = grid.row(grid.maxRow).indexWhere(_ == '.')
    val start = Pos(grid.minRow, startCol)
    val goal  = Pos(grid.maxRow, goalCol)
    longestPath(grid, goal, 0, Queue((start, 0, Set.empty)))

  @tailrec
  private def longestPath(grid: Grid, goal: Pos, max: Int, queue: Queue[(Pos, Int, Set[Pos])]): Int =
    queue.dequeueOption match
      case None => max
      case Some(((pos, length, visited), remaining)) =>
        if pos == goal then
          longestPath(grid, goal, Math.max(max, length), remaining)
        else
          longestPath(grid, goal, max, remaining ++ neighbors(grid, pos).filterNot(visited.contains).map(n => (n, length + 1, visited + pos)))

  private def neighbors(grid: Grid, pos: Pos): Set[Pos] =
    given Neighbors = NSEWNeighbors
    grid(pos) match
      case '>' => Set(pos.east)
      case '<' => Set(pos.west)
      case '^' => Set(pos.north)
      case 'v' => Set(pos.south)
      case _   => pos.neighbors.filter(grid.contains).filterNot(grid(_) == '#')
