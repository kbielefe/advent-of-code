package day21
import algorithms.{Grid, given}
import Grid.{Neighbors, Pos}
import parse.{*, given}

object Puzzle extends runner.Day[Grid, Long, Long]:
  def part1(grid: Grid): Long =
    val start = grid.find('S').get
    given Neighbors = Grid.NSEWNeighbors
    Iterator.iterate(Set(start))(_.flatMap(_.neighbors).filterNot(pos => grid(pos) == '#')).drop(64).next.size

  def part2(grid: Grid): Long =
    val start = grid.find('S').get
    given Neighbors = Grid.NSEWNeighbors
    val results = (1 to 5).map{i =>
      Iterator.iterate(Set(start))(_.flatMap(_.neighbors).filterNot{case Pos(row, col) => grid(Pos(((row % grid.maxRow) + grid.maxRow) % grid.maxRow, ((col % grid.maxCol) + grid.maxCol) % grid.maxCol)) == '#'}).drop(i * 64).next.size
    }
    val diff1 = results.sliding(2).map{case Seq(x, y) => y - x}
    val diff2 = diff1.sliding(2).map{case Seq(x, y) => y - x}
    diff2.foreach(println)
    ???
