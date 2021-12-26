package advent2021
import puzzleparse.{Grid, Pos}

object Day25:
  given CanEqual[Map[Pos, Char], Map[Pos, Char]] = CanEqual.derived

  def part1(input: Grid[Char]): Int =
    val grid = input.filterNot(_._2 == '.')
    val width = grid.map(_._1.col).max + 1
    val height = grid.map(_._1.row).max + 1
    Iterator.iterate[Map[Pos, Char]](grid)(step(width, height)).sliding(2).indexWhere{case Seq(x, y) => x == y} + 1

  private def step(width: Int, height: Int)(grid: Map[Pos, Char]): Map[Pos, Char] =
    val eastFacing = grid.filter(_._2 == '>')
    val southFacing = grid.filter(_._2 == 'v')
    val eastMoved = eastFacing.map{case (Pos(row, col), _) =>
      val east = Pos(row, (col + 1) % width)
      if grid.contains(east) then (Pos(row, col) -> '>') else (east -> '>')
    }.toMap
    val southMoved = southFacing.map{case (Pos(row, col), _) =>
      val south = Pos((row + 1) % height, col)
      if eastMoved.contains(south) || southFacing.contains(south) then (Pos(row, col) -> 'v') else (south -> 'v')
    }.toMap
    eastMoved ++ southMoved

  def part2(input: Grid[Char]): Int =
    ???
