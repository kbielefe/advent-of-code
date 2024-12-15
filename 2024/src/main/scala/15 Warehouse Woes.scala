package day15
import algorithms.{Grid, given}, Grid.{Pos, Dir}
import parse.{*, given}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Warehouse(grid: Grid, moves: List[Dir])
given Read[List[Dir]] = summon[Read[String]].map: string =>
  string.toList.flatMap:
    case '^' => Some(Dir.North)
    case '>' => Some(Dir.East)
    case 'v' => Some(Dir.South)
    case '<' => Some(Dir.West)
    case _   => None

given Read[Warehouse] = Read("\n\n")

object Puzzle extends runner.Day[Warehouse, Int, Int]:
  def part1(warehouse: Warehouse): Int =
    gps(moveToEnd(warehouse.grid, warehouse.moves))

  def part2(warehouse: Warehouse): Int =
    gps(moveToEnd(expand(warehouse.grid), warehouse.moves))

  def gps(grid: Grid): Int =
    (grid.findAll('O') ++ grid.findAll('[')).map(pos => pos.row * 100 + pos.col).sum

  def expand(grid: Grid): Grid =
    def expanded(pos: Pos, chars: String): List[(Pos, Char)] =
      List((Pos(pos.row, pos.col * 2), chars(0)), (Pos(pos.row, pos.col * 2 + 1), chars(1)))

    val expandedCells = grid.cells.flatMap:
      case (pos, '#') => expanded(pos, "##")
      case (pos, 'O') => expanded(pos, "[]")
      case (pos, '.') => expanded(pos, "..")
      case (pos, '@') => expanded(pos, "@.")
      case _ => ???
    Grid(expandedCells)

  def moveToEnd(grid: Grid, moves: List[Dir]): Grid =
    moves.foldLeft(grid): (grid, dir) =>
      val moved = allMoved(Queue(grid.find('@').get), grid, dir, Set.empty)
      if moved.exists(pos => grid(pos.moveInDir(dir)) == '#') then
        grid
      else
        move(grid, moved, dir)

  def move(grid: Grid, moved: Set[Pos], dir: Dir): Grid =
    val newEmpty = moved -- moved.map(_.moveInDir(dir))
    grid ++ newEmpty.map(_ -> '.') ++ moved.map(pos => pos.moveInDir(dir) -> grid(pos))

  @tailrec
  def allMoved(queue: Queue[Pos], grid: Grid, dir: Dir, visited: Set[Pos]): Set[Pos] =
    queue.dequeueOption match
      case Some(pos, rest) =>
        val char = grid(pos)
        val nextEnqueued = if "@O[]".contains(char) && !visited.contains(pos.moveInDir(dir)) then rest.enqueue(pos.moveInDir(dir)) else rest
        val newVisited = if "@O[]".contains(char) then visited + pos else visited
        val boxHalfEnqueued =
          if char == '[' && !visited.contains(pos.east) then
            nextEnqueued.enqueue(pos.east)
          else if char == ']'  && !visited.contains(pos.west) then
            nextEnqueued.enqueue(pos.west)
          else
            nextEnqueued
        allMoved(boxHalfEnqueued, grid, dir, newVisited)
      case None => visited
