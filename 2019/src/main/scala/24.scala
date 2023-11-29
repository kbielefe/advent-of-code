package day24

import parse.{*, given}
import algorithms.{*, given}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    def grids = Iterator.iterate(input)(life)
    val Some(x -> _) = detectCycle(grids, 0): @unchecked
    val firstRepeated = grids.drop(x).next
    biodiversityRating(firstRepeated)

  def part2(input: Grid): Int =
    ???

  def biodiversityRating(grid: Grid): Int =
    val positions = for
      row <- (grid.minRow to grid.maxRow).iterator
      col <- (grid.minCol to grid.maxCol).iterator
    yield Grid.Pos(row, col)
    val powersOfTwo = Iterator.iterate(1)(_ * 2)
    positions.zip(powersOfTwo).map((pos, power) => if grid(pos) == '#' then power else 0).sum

  def life(grid: Grid): Grid =
    grid.allPos.foldLeft(grid){(accum, pos) =>
      val current = grid(pos)
      val count = grid.neighborCount(pos)
      val newChar = if current == '#' && count != 1 then
        '.'
      else if current == '.' && (count == 1 || count == 2) then
        '#'
      else
        current
      accum.updated(pos, newChar)
    }
