package day17
import parse.{*, given}
import algorithms.{AStar, Grid, given}
import fs2.Stream
import Grid.Pos
import visualizations.*

case class Crucible(pos: Pos, direction: Char, distance: Int)

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    val startEast =  Crucible(Pos(0, 0), 'e', 1)
    val startSouth = Crucible(Pos(0, 0), 's', 1)
    val goal = Pos(grid.maxRow, grid.maxCol)
    val astar = new AStar[Crucible, Int](_.pos == goal, _.pos.manhattan(goal), (_, neighbor) => grid(neighbor.pos).asDigit, 0, getNeighbors(grid))
    astar.getMinCost(startEast, startSouth).get

  def part2(grid: Grid): Int =
    val startEast =  Crucible(Pos(0, 0), 'e', 1)
    val startSouth = Crucible(Pos(0, 0), 's', 1)
    val goal = Pos(grid.maxRow, grid.maxCol)
    val astar = new AStar[Crucible, Int](c => c.pos == goal && c.distance >= 4, _.pos.manhattan(goal), (_, neighbor) => grid(neighbor.pos).asDigit, 0, ultraNeighbors(grid))
    astar.getMinCost(startEast, startSouth).get

  def steps(grid: Grid): Unit =
    val startEast =  Crucible(Pos(0, 0), 'e', 1)
    val startSouth = Crucible(Pos(0, 0), 's', 1)
    val goal = Pos(grid.maxRow, grid.maxCol)
    val astar = new AStar[Crucible, Int](c => c.pos == goal && c.distance >= 4, _.pos.manhattan(goal), (_, neighbor) => grid(neighbor.pos).asDigit, 0, ultraNeighbors(grid))
    val frames = astar.visualize(startEast, startSouth).map:
      case astar.Visited(Crucible(pos, dir, _)) =>
        Frame(Map((pos.row,pos.col) -> Seq(ChangeColor("red"), ChangeChar(dirToArrow(dir)))))
      case astar.Opened(crucibles) =>
        Frame(crucibles.map(c => ((c.pos.row,c.pos.col), Seq(ChangeColor("blue"), ChangeChar(dirToArrow(c.direction))))).toMap)
      case astar.FoundPath(path) =>
        Frame(path.map(c => ((c.pos.row,c.pos.col), Seq(ChangeColor("green"), ChangeChar(dirToArrow(c.direction))))).toMap)
      case astar.Failed => ???
    AnimatedGrid(grid, frames)

  private def dirToArrow(dir: Char): Char = dir match
    case 'n' => '↑'
    case 's' => '↓'
    case 'e' => '→'
    case 'w' => '←'
    case _ => ???

  def getNeighbors(grid: Grid)(crucible: Crucible): Set[Crucible] =
    val Crucible(pos, dir, dist) = crucible
    val all: Set[Crucible] = (dir, dist == 3) match
      case ('n',  true) => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1))
      case ('s',  true) => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1))
      case ('e',  true) => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1))
      case ('w',  true) => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1))
      case ('n', false) => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1), Crucible(pos.north, 'n', dist + 1))
      case ('s', false) => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1), Crucible(pos.south, 's', dist + 1))
      case ('e', false) => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1), Crucible(pos.east,  'e', dist + 1))
      case ('w', false) => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1), Crucible(pos.west,  'w', dist + 1))
      case (char, _) => throw new Exception(s"unknown direction: $char")
    all.filter(crucible => grid.contains(crucible.pos))

  def ultraNeighbors(grid: Grid)(crucible: Crucible): Set[Crucible] =
    val Crucible(pos, dir, dist) = crucible
    val all: Set[Crucible] =
      if dist < 4 then
        dir match
          case 'n' => Set(Crucible(pos.north, 'n', dist + 1))
          case 's' => Set(Crucible(pos.south, 's', dist + 1))
          case 'e' => Set(Crucible(pos.east,  'e', dist + 1))
          case 'w' => Set(Crucible(pos.west,  'w', dist + 1))
      else if dist == 10 then
        dir match
          case 'n' => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1))
          case 's' => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1))
          case 'e' => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1))
          case 'w' => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1))
      else
        dir match
          case 'n' => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1), Crucible(pos.north, 'n', dist + 1))
          case 's' => Set(Crucible(pos.east,  'e', 1), Crucible(pos.west,  'w', 1), Crucible(pos.south, 's', dist + 1))
          case 'e' => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1), Crucible(pos.east,  'e', dist + 1))
          case 'w' => Set(Crucible(pos.north, 'n', 1), Crucible(pos.south, 's', 1), Crucible(pos.west,  'w', dist + 1))
    all.filter(crucible => grid.contains(crucible.pos))
