package advent2021
import puzzleparse.{Grid, Pos}
import scala.annotation.tailrec

object Day9:
  def part1(input: Grid[Char]): Int =
    val grid = input.mapValues(_.asDigit).toMap.asInstanceOf[Grid[Int]]
    grid.lowPoints.map(grid.risk).sum

  def part2(input: Grid[Char]): Int =
    val grid = input.mapValues(_.asDigit).toMap.asInstanceOf[Grid[Int]]
    grid.lowPoints.map(grid.basinSize).sorted.takeRight(3).product

  extension (pos: Pos)
    def neighbors: Set[Pos] = Set(
      Pos(pos.row - 1, pos.col),
      Pos(pos.row + 1, pos.col),
      Pos(pos.row, pos.col - 1),
      Pos(pos.row, pos.col + 1)
    )

  extension (grid: Grid[Int])
    def risk(pos: Pos): Int = grid(pos) + 1

    def isLowPoint(pos: Pos): Boolean =
      pos.neighbors.filter(grid.contains).forall(neighbor => grid(neighbor) > grid(pos))

    def lowPoints: List[Pos] =
      grid.keys.filter(grid.isLowPoint).toList

    def basinSize(pos: Pos): Int =
      basin(Set(pos)).size

    @tailrec
    def basin(acc: Set[Pos]): Set[Pos] =
      val fill = acc ++ acc.flatMap(_.neighbors).filter(grid.contains).filter(grid(_) != 9)
      if fill.size == acc.size then
        acc
      else
        basin(fill)
