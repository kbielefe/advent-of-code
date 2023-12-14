package day14
import algorithms.{*, given}
import Grid.Pos
import parse.{*, given}

extension (grid: Grid)
  def tip: Grid =
    val roundRocks = for
      row <- (grid.minRow to grid.maxRow).iterator
      col <- (grid.minCol to grid.maxCol).iterator
      pos  = Pos(row, col)
      if grid(pos) == 'O'
    yield pos
    roundRocks.foldLeft(grid){(grid, rock) =>
      val newRow = ((rock.row - 1) to grid.minRow by -1).takeWhile(row => grid(Pos(row, rock.col)) == '.').lastOption.getOrElse(rock.row)
      grid + (rock -> '.') + (Pos(newRow, rock.col) -> 'O')
    }

  def load: Int =
    grid.allPos.iterator.filter(pos => grid(pos) == 'O').map(pos => grid.maxRow - pos.row + 1).sum

  def cycle: Grid =
    val t = Matrix.reflectX[Int] * Matrix.rotateCW[Int] * Matrix.reflectX[Int]
    grid.tip.transform(t).tip.transform(t).tip.transform(t).tip.transform(t)

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    grid.tip.load

  def part2(grid: Grid): Int =
    val Some((init, period)) = detectCycle(Iterator.iterate(grid)(_.cycle), 0): @unchecked
    val needToDrop = cycledEquivalentIterations(init, period, 1000000000L).toInt
    Iterator.iterate(grid)(_.cycle).drop(needToDrop).next.load
