package day6
import algorithms.{Grid, given}, Grid.{Dir, PosDir}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    guardPath(grid).map(_.pos).distinct.size

  def part2(grid: Grid): Int =
    val start = guardPath(grid).next.pos
    val potentialObstacles = guardPath(grid).map(_.moveForward.pos).filter(grid.get(_) == Some('.')).distinct
    potentialObstacles.count(obstacle => hasLoop(grid + (obstacle -> '#')))

  def hasLoop(grid: Grid): Boolean =
    val visited = guardPath(grid).scanLeft(Set.empty[PosDir])(_ + _)
    guardPath(grid).zip(visited).exists{case (posDir, visited) => visited.contains(posDir)}

  def guardPath(grid: Grid): Iterator[PosDir] =
    val start = PosDir(grid.find('^').get, Dir.North)
    Iterator.iterate(start): posDir =>
        if grid.get(posDir.moveForward.pos) == Some('#') then posDir.turnRight else posDir.moveForward
      .takeWhile(posDir => grid.contains(posDir.pos))
