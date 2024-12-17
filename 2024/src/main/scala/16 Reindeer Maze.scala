package day16
import algorithms.{AStar, Grid, given}, Grid.{Pos, Dir, PosDir}
import parse.{*, given}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    astar(grid, 'S', _.moveForward).getMinCost(PosDir(grid.find('S').get, Dir.East)).get

  def part2(grid: Grid): Int =
    val start = grid.find('S').get
    val end = grid.find('E').get
    val Some((lowestCostForward, _, goal)) = astar(grid, 'E', _.moveForward).calculate(PosDir(start, Dir.East)): @unchecked
    val Some((lowestCostBackward, _, _)) = astar(grid, 'S', _.moveBackward).calculate(Dir.values.map(dir => PosDir(end, dir))*): @unchecked
    val minCost = lowestCostForward(goal)
    lowestCostForward.filter((key, value) => lowestCostBackward.getOrElse(key, Int.MaxValue) + value == minCost).toSet.map(_._1.pos).size

  def astar(grid: Grid, goalChar: Char, move: PosDir => PosDir): AStar[PosDir, Int] =
    val goal = grid.find(goalChar).get
    def neighborWeight(current: PosDir, neighbor: PosDir): Int =
      if current.dir != neighbor.dir then 1000 else 1
    def getNeighbors(current: PosDir): Set[PosDir] =
      Set(
        current.turnLeft,
        current.turnRight,
        move(current)
      ).filter(posDir => grid(posDir.pos) != '#')

    new AStar[PosDir, Int](
      _.pos == goal,
      _.pos.manhattan(goal),
      neighborWeight,
      0,
      getNeighbors
    )
