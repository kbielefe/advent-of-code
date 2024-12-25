package day25
import algorithms.{Grid, given}, Grid.Pos
import parse.{*, given}

given Read[List[Grid]] = Read("\n\n")

extension (grid: Grid)
  def isLock: Boolean =
    grid(Pos(0, 0)) == '#'
  
  def fits(lock: Grid): Boolean =
    (0 to grid.width).forall: col =>
      keyHeight(col) + lock.lockHeight(col) <= grid.height - 2

  def keyHeight(col: Int): Int =
    grid.col(col).dropWhile(_ == '.').drop(1).size

  def lockHeight(col: Int): Int =
    grid.col(col).drop(1).takeWhile(_ == '#').size

object Puzzle extends runner.Day[List[Grid], Int, Int]:
  def part1(grids: List[Grid]): Int =
    val (locks, keys) = grids.partition(_.isLock)
    val fits = for
      lock <- locks
      key <- keys
    yield key.fits(lock)
    fits.count(identity)

  def part2(grids: List[Grid]): Int =
    ???
