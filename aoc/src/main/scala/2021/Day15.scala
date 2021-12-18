package advent2021
import puzzleparse.{Grid, Pos}
import algorithms.AStar

object Day15:
  def part1(input: Grid[Int]): Int =
    totalRisk(input)

  def part2(input: Grid[Int]): Int =
    totalRisk(expandGrid(input))

  private def totalRisk(input: Grid[Int]): Int =
    val astar = new AStar(heuristic, weight(input), 0, neighbors(input))
    val goal = Pos(input.keySet.map(_.row).max, input.keySet.map(_.col).max)
    val path = astar.getPath(Pos(0, 0), goal)
    path.drop(1).map(input(_)).sum

  private def expandGrid(grid: Grid[Int]): Grid[Int] =
    val maxRow = grid.keySet.map(_.row).max
    val maxCol = grid.keySet.map(_.col).max
    val expanded = for
      tileRow <- 0 to 4
      tileCol <- 0 to 4
      row     <- 0 to maxRow
      col     <- 0 to maxCol
    yield tile(maxRow, maxCol, tileRow, tileCol, row, col, grid)
    expanded.toMap.asInstanceOf[Grid[Int]]

  private def tile(maxRow: Int, maxCol: Int, tileRow: Int, tileCol: Int, row: Int, col: Int, grid: Grid[Int]): (Pos, Int) =
    val newRow = tileRow * (maxRow + 1) + row
    val newCol = tileCol * (maxCol + 1) + col
    val unwrappedRisk = grid(Pos(row, col)) + tileRow + tileCol
    val wrappedRisk = if unwrappedRisk % 9 == 0 then 9 else unwrappedRisk % 9
    (Pos(newRow, newCol), wrappedRisk)

  private def heuristic(from: Pos, to: Pos): Int =
    (from.row - to.row).abs + (from.col - to.col).abs

  private def weight(grid: Grid[Int])(from: Pos, to: Pos): Int =
    grid(to)

  private def neighbors(grid: Grid[Int])(from: Pos): Set[Pos] =
    Set(Pos(from.row - 1, from.col),
        Pos(from.row + 1, from.col),
        Pos(from.row, from.col - 1),
        Pos(from.row, from.col + 1)
    ).filter(grid contains _)
