package day16
import algorithms.{Grid, given}
import Grid.Pos
import parse.{*, given}
import scala.annotation.tailrec

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(grid: Grid): Int =
    energized(grid, Set.empty, Set((Pos(0, 0), 'e'))).size

  def part2(grid: Grid): Int =
    val starts =
      (grid.minCol to grid.maxCol).map(col => (Pos(0, col) -> 's')).toSet ++
      (grid.minCol to grid.maxCol).map(col => (Pos(grid.maxRow, col) -> 'n')).toSet ++
      (grid.minRow to grid.maxRow).map(row => (Pos(row, 0) -> 'e')).toSet ++
      (grid.minRow to grid.maxRow).map(row => (Pos(row, grid.maxCol) -> 'w')).toSet
    starts.map(start => energized(grid, Set.empty, Set(start)).size).max

  case class Move(continue: Char, slash: Char, backSlash: Char, split: Char)
  val moves = Map(
    'n' -> Move('n', 'e', 'w', '-'),
    's' -> Move('s', 'w', 'e', '-'),
    'e' -> Move('e', 'n', 's', '|'),
    'w' -> Move('w', 's', 'n', '|')
  )

  def move(pos: Pos, dir: Char): Pos = dir match
    case 'n' => pos.north
    case 'e' => pos.east
    case 's' => pos.south
    case 'w' => pos.west

  @tailrec
  def energized(grid: Grid, visited: Set[(Pos, Char)], toVisit: Set[(Pos, Char)]): Set[Pos] =
    toVisit.headOption match
      case None => visited.map(_._1)
      case Some((pos, dir)) =>
        grid.get(pos) match
          case None =>
            energized(grid, visited, toVisit - (pos -> dir) -- visited)
          case Some('.')  =>
            val moveDir = moves(dir).continue
            energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moveDir) -> moveDir) -- visited)
          case Some('/')  =>
            val moveDir = moves(dir).slash
            energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moveDir) -> moveDir) -- visited)
          case Some('\\') =>
            val moveDir = moves(dir).backSlash
            energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moveDir) -> moveDir) -- visited)
          case Some('-') =>
            if moves(dir).split == '-' then
              energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moves(dir).slash) -> moves(dir).slash) + (move(pos, moves(dir).backSlash) -> moves(dir).backSlash) -- visited)
            else
              val moveDir = moves(dir).continue
              energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moveDir) -> moveDir) -- visited)
          case Some('|')  =>
            if moves(dir).split == '|' then
              energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moves(dir).slash) -> moves(dir).slash) + (move(pos, moves(dir).backSlash) -> moves(dir).backSlash) -- visited)
            else
              val moveDir = moves(dir).continue
              energized(grid, visited + (pos -> dir), toVisit - (pos -> dir) + (move(pos, moveDir) -> moveDir) -- visited)
          case Some(char) => throw new Exception(s"unknown char: $char")
