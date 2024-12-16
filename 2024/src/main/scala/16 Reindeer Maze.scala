package day16
import algorithms.{AStar, Grid, given}, Grid.{Pos, Dir, PosDir}
import parse.{*, given}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    astar(grid).getMinCost(PosDir(grid.find('S').get, Dir.East)).get

  def part2(grid: Grid): Int =
    astar(grid).getAllPositionsOnShortestPaths(PosDir(grid.find('S').get, Dir.East)).map(_.pos).size

  def astar(grid: Grid): AStar[PosDir, Int] =
    val goal = grid.find('E').get
    def neighborWeight(current: PosDir, neighbor: PosDir): Int =
      if current.dir != neighbor.dir then 1000 else 1
    def getNeighbors(current: PosDir): Set[PosDir] =
      Set(
        current.turnLeft,
        current.turnRight,
        current.moveForward
      ).filter(posDir => grid(posDir.pos) != '#')

    new AStar[PosDir, Int](
      _.pos == goal,
      _.pos.manhattan(goal),
      neighborWeight,
      0,
      getNeighbors
    )
