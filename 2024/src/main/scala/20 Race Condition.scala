package day20
import algorithms.{Grid, given}, Grid.*

object Puzzle extends runner.Day[Grid, Int, Int]:
  given Neighbors = NSEWNeighbors
  def part1(grid: Grid): Int =
    answer(grid, 2)

  def part2(grid: Grid): Int =
    answer(grid, 20)

  def answer(grid: Grid, maxCheatDistance: Int): Int =
    val start = grid.find('S').get
    val goal = grid.find('E').get
    val Some((distances, _, _)) = grid.aStar(goal).calculate(start): @unchecked
    val savings = for
      start <- distances.keysIterator
      end <- for
        rowOffset <- (-maxCheatDistance to maxCheatDistance).iterator
        colOffset <- (Math.abs(rowOffset) - maxCheatDistance to maxCheatDistance - Math.abs(rowOffset)).iterator
        end = Pos(start.row + rowOffset, start.col + colOffset)
        if distances.contains(end)
      yield end
    yield (distances(end) - distances(start), start.manhattan(end))
    savings.count((distance, cheatDistance) => cheatDistance <= maxCheatDistance && distance - cheatDistance >= 100)
