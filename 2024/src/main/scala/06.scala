package day6
import algorithms.{Grid, lastOption, given}, Grid.{Pos, Dir, PosDir}
import scala.annotation.tailrec

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    findGuardPath(grid, firstObstacleInDir(grid)).map(_.pos).toSet.size

  def part2(grid: Grid): Int =
    val obstacles = firstObstacleInDir(grid)
    val start = grid.find('^').get
    (findGuardPath(grid, obstacles).flatMap(obstacleHasLoop(grid, obstacles, PosDir(start, Dir.North))).toSet - start).size

  def obstacleHasLoop(grid: Grid, obstacles: Map[PosDir, PosDir], start: PosDir)(guard: PosDir): Option[Pos] =
    val obstaclePos = guard.pos.moveInDir(guard.dir)
    val newObstacles = addObstacle(grid, obstaclePos, obstacles)
    @tailrec
    def helper(visited: Set[PosDir], current: PosDir): Option[Pos] =
      if visited.contains(current) then
        Some(obstaclePos)
      else newObstacles.get(current) match
        case Some(next) => helper(visited + current, next)
        case None => None
    grid.get(obstaclePos) match
      case Some('#') => None
      case _         => helper(Set.empty, start)
  end obstacleHasLoop

  def addObstacle(grid: Grid, pos: Pos, obstacles: Map[PosDir, PosDir]): Map[PosDir, PosDir] =
    def entries = Iterator(Dir.North, Dir.South, Dir.East, Dir.West).flatMap: dir =>
      val dest = pos.moveInDir(dir)
      Iterator.iterate(dest)(_.moveInDir(dir))
        .takeWhile: pos =>
          grid.get(pos) match
            case Some('#') => false
            case None      => false
            case _         => true
        .map(pos => PosDir(pos, dir.opposite) -> PosDir(dest, dir.opposite.turnRight))

    grid.get(pos) match
      case Some('#') => obstacles
      case None      => obstacles
      case _         =>
        obstacles ++ entries
  end addObstacle

  def findGuardPath(grid: Grid, obstacles: Map[PosDir, PosDir]): Iterator[PosDir] =
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
    (path ++ Iterator(last)).sliding(2).flatMap{case Seq(left, right) => allPos(left, right)}
  end findGuardPath

  def allPos(left: PosDir, right: PosDir): Iterator[PosDir] =
    (left.pos.row to right.pos.row by (if right.pos.row - left.pos.row < 1 then -1 else 1)).iterator.flatMap: row =>
      (left.pos.col to right.pos.col by (if right.pos.col - left.pos.col < 1 then -1 else 1)).iterator.map: col =>
        val pos = Pos(row, col)
        PosDir(pos, if pos == right.pos then right.dir else left.dir)
  end allPos

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
