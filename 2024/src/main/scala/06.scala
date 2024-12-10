package day6
import algorithms.{Grid, given}, Grid.{Dir, Pos, PosDir}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    val start = PosDir(grid.find('^').get, Dir.North)
    guardPath(grid, start).map(_.pos).distinct.size

  def part2(grid: Grid): Int =
    val start = PosDir(grid.find('^').get, Dir.North)
    val visited = guardPath(grid, start).map(_.moveForward.pos).scanLeft(Set.empty[Pos])(_ + _)
    val potentialObstacles = guardPath(grid, start).zip(visited).filter: (posDir, visited) =>
        val obstacle = posDir.moveForward.pos
        !visited.contains(obstacle) && grid.get(obstacle) == Some('.')
      .map(_._1)
    potentialObstacles.count(start => hasLoop(grid + (start.moveForward.pos -> '#'), start))

  def hasLoop(grid: Grid, start: PosDir): Boolean =
    val visited = guardPath(grid, start).scanLeft(Set.empty[PosDir])(_ + _)
    guardPath(grid, start).zip(visited).exists{case (posDir, visited) => visited.contains(posDir)}

  def guardPath(grid: Grid, start: PosDir): Iterator[PosDir] =
    Iterator.iterate(start): posDir =>
        if grid.get(posDir.moveForward.pos) == Some('#') then posDir.turnRight else posDir.moveForward
      .takeWhile(posDir => grid.contains(posDir.pos))
