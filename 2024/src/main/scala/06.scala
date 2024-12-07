package day6
import algorithms.{Grid, lastOption, given}, Grid.{Pos, Dir, PosDir}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    val obstacles = firstObstacleInDir(grid)
    val start: Option[PosDir] = Some(PosDir(grid.find('^').get, Dir.North))
    def path = Iterator.iterate(start):
        case Some(posDir) => obstacles.get(posDir)
        case None => None
      .takeWhile(_.isDefined)
      .flatten
    val last = path.lastOption.get match
      case PosDir(pos, Dir.North) => PosDir(Pos(grid.minRow, pos.col), Dir.North)
      case PosDir(pos, Dir.South) => PosDir(Pos(grid.maxRow, pos.col), Dir.South)
      case PosDir(pos, Dir.East)  => PosDir(Pos(pos.row, grid.maxCol), Dir.East)
      case PosDir(pos, Dir.West)  => PosDir(Pos(pos.row, grid.minCol), Dir.West)
    (path ++ Iterator(last)).sliding(2).flatMap{case Seq(left, right) => allPos(left, right)}.toSet.size

  def part2(grid: Grid): Int =
    0

  def allPos(left: PosDir, right: PosDir): Set[Pos] =
    (left.pos.row to right.pos.row by (if right.pos.row - left.pos.row < 1 then -1 else 1)).toSet.flatMap: row =>
      (left.pos.col to right.pos.col by (if right.pos.col - left.pos.col < 1 then -1 else 1)).toSet.map: col =>
        Pos(row, col)

  def firstObstacleInDir(grid: Grid): Map[PosDir, PosDir] =
    val closestNorth: Iterator[Map[Int, Int]] = grid.rows.zipWithIndex.scanLeft(Map.empty[Int, Int]):
      case (accum, (row, rowNumber)) => accum ++ row.zipWithIndex.filter(_._1 == '#').map(_._2 -> rowNumber)
    val north = (grid.minRow to grid.maxRow).iterator.zip(closestNorth).flatMap: (row, closest) =>
      (grid.minCol to grid.maxCol).iterator.flatMap: col =>
        closest.get(col).map: closest =>
          PosDir(Pos(row, col), Dir.North) -> PosDir(Pos(closest + 1, col), Dir.East)

    val closestSouth: Iterator[Map[Int, Int]] = grid.reverseRows.zipWithIndex.scanLeft(Map.empty[Int, Int]):
      case (accum, (row, rowNumber)) => accum ++ row.zipWithIndex.filter(_._1 == '#').map(_._2 -> (grid.maxRow - rowNumber))
    val south = (grid.maxRow to grid.minRow by -1).iterator.zip(closestSouth).flatMap: (row, closest) =>
      (grid.minCol to grid.maxCol).iterator.flatMap: col =>
        closest.get(col).map: closest =>
          PosDir(Pos(row, col), Dir.South) -> PosDir(Pos(closest - 1, col), Dir.West)

    val closestWest: Iterator[Map[Int, Int]] = grid.cols.zipWithIndex.scanLeft(Map.empty[Int, Int]):
      case (accum, (col, colNumber)) => accum ++ col.zipWithIndex.filter(_._1 == '#').map(_._2 -> colNumber)
    val west = (grid.minCol to grid.maxCol).iterator.zip(closestWest).flatMap: (col, closest) =>
      (grid.minRow to grid.maxRow).iterator.flatMap: row =>
        closest.get(row).map: closest =>
          PosDir(Pos(row, col), Dir.West) -> PosDir(Pos(row, closest + 1), Dir.North)

    val closestEast: Iterator[Map[Int, Int]] = grid.reverseCols.zipWithIndex.scanLeft(Map.empty[Int, Int]):
      case (accum, (col, colNumber)) => accum ++ col.zipWithIndex.filter(_._1 == '#').map(_._2 -> (grid.maxCol - colNumber))
    val east = (grid.maxCol to grid.minCol by -1).iterator.zip(closestEast).flatMap: (col, closest) =>
      (grid.minRow to grid.maxRow).iterator.flatMap: row =>
        closest.get(row).map: closest =>
          PosDir(Pos(row, col), Dir.East) -> PosDir(Pos(row, closest - 1), Dir.South)

    (north ++ south ++ east ++ west).toMap
  end firstObstacleInDir
