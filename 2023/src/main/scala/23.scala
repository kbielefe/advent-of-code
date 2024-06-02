package day23
import algorithms.{*, given}
import algorithms.Graph.*
import algorithms.Grid.*
import parse.{*, given}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    given Neighbors = NSEWNeighbors

    val startCol = grid.row(grid.minRow).indexWhere(_ == '.')
    val goalCol  = grid.row(grid.maxRow).indexWhere(_ == '.')
    val start = Pos(grid.minRow, startCol)
    val goal  = Pos(grid.maxRow, goalCol)

    def isNeighbor(pos: Pos)(neighbor: Pos): Boolean =
      grid.get(neighbor) match
        case None => false
        case Some(char) => grid(pos) match
          case '<' => neighbor.col < pos.col && char != '#'
          case '>' => neighbor.col > pos.col && char != '#'
          case '^' => neighbor.row < pos.row && char != '#'
          case 'v' => neighbor.row > pos.row && char != '#'
          case _ => char != '#'

    given Graph[Pos] = new Graph[Pos]:
      def neighbors(from: Pos): Iterator[Pos] =
        from.neighbors.iterator.filter(isNeighbor(from))

    start.toposort.foreach(println)
    //start.longestPaths(goal)
    ???
  end part1

  def part2(grid: Grid): Int =
    ???
