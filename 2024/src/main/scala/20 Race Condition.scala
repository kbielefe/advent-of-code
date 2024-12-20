package day20
import algorithms.{Grid, given}, Grid.*

object Puzzle extends runner.Day[Grid, Int, Int]:
  given Neighbors = NSEWNeighbors
  def part1(grid: Grid): Int =
    val start = grid.find('S').get
    val goal = grid.find('E').get
    val Some((distances, _, _)) = grid.aStar(goal).calculate(start): @unchecked
    def race = grid.findAll('.') ++ Iterator(start) ++ Iterator(goal)
    val verticalObstacles = race.map(pos => (pos, pos.north, pos.north.north)).filter((first, obstacle, second) => grid(obstacle) == '#' && distances.get(second).map(d => Math.abs(d - distances(first)) >= 102).getOrElse(false))
    val horizontalObstacles = race.map(pos => (pos, pos.east, pos.east.east)).filter((first, obstacle, second) => grid(obstacle) == '#' && distances.get(second).map(d => Math.abs(d - distances(first)) >= 102).getOrElse(false))
    println(distances.size)
    verticalObstacles.size + horizontalObstacles.size

  def part2(grid: Grid): Int =
    ???
