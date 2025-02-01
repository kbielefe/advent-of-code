package advent2022
import puzzleparse.{Grid, Digit, Pos}

object Day8:
  def part1(input: Grid[Digit]): Int =
    val maxCol = input.map(_._1.col).max
    val maxRow = input.map(_._1.row).max
    (visible(input, 0 to maxRow,       0 to maxCol, maxCol, true) |
     visible(input, maxRow to 0 by -1, 0 to maxCol, maxCol, true) |
     visible(input, 0 to maxCol,       0 to maxRow, maxRow, false) |
     visible(input, maxCol to 0 by -1, 0 to maxRow, maxRow, false)).size

  def part2(input: Grid[Digit]): Int =
    val maxCol = input.map(_._1.col).max
    val maxRow = input.map(_._1.row).max
    input.keysIterator.map(scenic(input, maxRow, maxCol)).max

  def visible(grid: Grid[Digit], xRange: Range, yRange: Range, width: Int, xIsRow: Boolean): Set[Pos] =
    def pos(x: Int, y: Int): Pos = if xIsRow then Pos(x, y) else Pos(y, x)
    val initialHeights = Vector.fill(width + 1)(-1)
    xRange.foldLeft((initialHeights, Set.empty[Pos])){
      case ((heights, accum), x) =>
        val newHeights = yRange.map(y => List(heights(y), grid(pos(x, y))).max).toVector
        val newAccum = accum ++ yRange.toSet.flatMap(y => if heights(y) < grid(pos(x, y)) then Set(pos(x, y)) else Set.empty)
        (newHeights, newAccum)
    }._2

  def scenic(heights: Grid[Digit], maxRow: Int, maxCol: Int)(pos: Pos): Int =
    val myHeight = heights(pos)
    val upView    = pos.row - Iterator.range(pos.row - 1, -1, -1).map(Pos(_, pos.col)).find(heights(_) >= myHeight).map(_.row).getOrElse(0)
    val downView  = Iterator.range(pos.row + 1, maxRow + 1, 1).map(Pos(_, pos.col)).find(heights(_) >= myHeight).map(_.row).getOrElse(maxRow) - pos.row
    val leftView  = pos.col - Iterator.range(pos.col - 1, -1, -1).map(Pos(pos.row, _)).find(heights(_) >= myHeight).map(_.col).getOrElse(0)
    val rightView = Iterator.range(pos.col + 1, maxCol + 1, 1).map(Pos(pos.row, _)).find(heights(_) >= myHeight).map(_.col).getOrElse(maxCol) - pos.col
    upView * downView * leftView * rightView
