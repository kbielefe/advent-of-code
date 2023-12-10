package day10
import parse.{*, given}
import algorithms.{Grid, floodFill, given}
import Grid.{Neighbors, Pos}

object Puzzle extends runner.Day[Grid, Int, Int]:
  def part1(input: Grid): Int =
    val grid = input.map(toBoxChars)
    val start = grid.find('S').get
    val size = Iterator.iterate(('W', start.east))(move(grid).tupled).map(_._2).takeWhile(_ != start).size
    math.ceil(size.toDouble / 2.0).toInt

  def part2(input: Grid): Int =
    val grid = input.map(toBoxChars)
    val start = grid.find('S').get
    val loop = Iterator.iterate(('W', start.east))(move(grid).tupled).map(_._2).takeWhile(_ != start).toSet + start
    val horiz = expandHorizontally(grid.keepOnlyPositionsIn(loop) + (start -> '─'))
    val vert = expandVertically(horiz)
    given Neighbors = Grid.NSEWNeighbors
    def neighbors(pos: Pos): Set[Pos] =
      pos.neighbors.filter(n => n.row >= (vert.minRow - 1) && n.row <= (vert.maxRow + 1) && n.col >= (vert.minCol - 1) && n.col <= (vert.maxCol + 1))
        .filter(n => vert.getOrElse(n, '.') == '.' || vert.getOrElse(n, '.') == ' ')
    val filled = floodFill(Pos(vert.minRow, vert.minCol), neighbors)
    println(vert -- filled)
    (vert -- filled).count(_ == ' ')

  def expandHorizontally(grid: Grid): Grid =
    val string = grid.rows.map{row =>
      row.head.toString + row.toList.sliding(2).map{case List(x, y) =>
        if "─└┌".contains(x) && "─┘┐".contains(y) then
          s"─$y"
        else
          s".$y"
      }.mkString
    }.mkString("\n")
    Grid(string)

  def expandVertically(grid: Grid): Grid =
    val cells: Map[Pos, Char] = (grid.minCol to grid.maxCol).flatMap{col =>
      (grid.minRow to grid.maxRow).flatMap{row =>
        val x = grid.getOrElse(Pos(row, col), ' ')
        val y = grid.getOrElse(Pos(row + 1, col), ' ')
        if "│┌┐".contains(x) && "│└┘".contains(y) then
          Set(Pos(row*2 + 1, col) -> '│', Pos(row*2 + 2, col) -> y)
        else
          Set(Pos(row*2 + 1, col) -> '.', Pos(row*2 + 2, col) -> y)
      } :+ (Pos(grid.minRow, col) -> grid.getOrElse(Pos(grid.minRow, col), ' '))
    }.toMap
    Grid(cells)

  def move(grid: Grid)(cameFrom: Char, pos: Pos): (Char, Pos) = (cameFrom, grid(pos)) match
    case ('N', '│') => ('N', pos.south)
    case ('W', '─') => ('W', pos.east)
    case ('N', '└') => ('W', pos.east)
    case ('N', '┘') => ('E', pos.west)
    case ('W', '┐') => ('N', pos.south)
    case ('E', '┌') => ('N', pos.south)
    case ('S', '│') => ('S', pos.north)
    case ('E', '─') => ('E', pos.west)
    case ('E', '└') => ('S', pos.north)
    case ('W', '┘') => ('S', pos.north)
    case ('S', '┐') => ('E', pos.west)
    case ('S', '┌') => ('W', pos.east)

  def toBoxChars(char: Char): Char = char match
    case '|' => '│'
    case '-' => '─'
    case 'L' => '└'
    case 'J' => '┘'
    case '7' => '┐'
    case 'F' => '┌'
    case x => x
