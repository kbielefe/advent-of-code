package day13
import parse.{*, given}
import algorithms.{*, given}
import algorithms.Grid.Pos

type I = List[Grid]
given Read[I] = Read("\n\n")

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(grids: I): Int =
    grids.flatMap(summary).sum

  def part2(grids: I): Int =
    grids.map(unsmudgedSummary).sum

  def unsmudgedSummary(grid: Grid): Int =
    val positions = for
      row <- (grid.minRow to grid.maxRow).iterator
      col <- (grid.minCol to grid.maxCol).iterator
    yield Pos(row, col)
    val grids = positions.map{pos =>
      val smudge = if grid(pos) == '#' then '.' else '#'
      grid + (pos -> smudge)
    }
    val originalSummary = summary(grid).head
    grids.flatMap(summary).find(_ != originalSummary).get

  def summary(grid: Grid): IndexedSeq[Int] =
    val colResult = ((grid.minCol + 1) to grid.maxCol).filter{col =>
      val flipped = grid.transform(Transforms.translate(col - 1, 0) * Transforms.reflectY * Transforms.translate(-col, 0))
      grid.overlapEquals(flipped)
    }
    val rowResult = ((grid.minRow + 1) to grid.maxRow).filter{row =>
      val flipped = grid.transform(Transforms.translate(0, row - 1) * Transforms.reflectX * Transforms.translate(0, -row))
      grid.overlapEquals(flipped)
    }.map(_ * 100)
    colResult ++ rowResult
