package advent2022
import algorithms.AStar
import puzzleparse.{Grid, Pos}

object Day24:
  def part1(input: Grid[Char]): Int =
    val lastRow = input.keys.map(_.row).max
    val lastCol = input.keys.map(_.col).max
    val startPos = input.find((pos, char) => pos.row == 0 && char == '.').get._1
    val endPos = input.find((pos, char) => pos.row == lastRow && char == '.').get._1
    val blizzards = Blizzards.fromGrid(input)
    val astar = new AStar[State, Int](_.pos == endPos, heuristic(endPos), (_, _) => 1, 0, getNeighbors(blizzards, lastRow, lastCol, startPos, endPos))
    astar.getMinCost(State(startPos, 0)).get

  def part2(input: Grid[Char]): Int =
    val lastRow = input.keys.map(_.row).max
    val lastCol = input.keys.map(_.col).max
    val startPos = input.find((pos, char) => pos.row == 0 && char == '.').get._1
    val endPos = input.find((pos, char) => pos.row == lastRow && char == '.').get._1
    val blizzards = Blizzards.fromGrid(input)
    val forwardAstar = new AStar[State, Int](_.pos == endPos, heuristic(endPos), (_, _) => 1, 0, getNeighbors(blizzards, lastRow, lastCol, startPos, endPos))
    val backAstar = new AStar[State, Int](_.pos == startPos, heuristic(startPos), (_, _) => 1, 0, getNeighbors(blizzards, lastRow, lastCol, startPos, endPos))
    val leg1 = forwardAstar.getMinCost(State(startPos, 0)).get
    val leg2 = backAstar.getMinCost(State(endPos, leg1)).get
    val leg3 = forwardAstar.getMinCost(State(startPos, leg1 + leg2)).get
    leg1 + leg2 + leg3

  case class State(pos: Pos, time: Int)

  private def heuristic(endPos: Pos)(state: State): Int =
    Math.abs(endPos.row - state.pos.row) + Math.abs(endPos.col - state.pos.col)

  private def getNeighbors(blizzards: Blizzards, maxRow: Int, maxCol: Int, start: Pos, goal: Pos)(state: State): Set[State] =
    val State(Pos(row, col), time) = state
    Set(
      State(Pos(row, col), time + 1),
      State(Pos(row + 1, col), time + 1),
      State(Pos(row - 1, col), time + 1),
      State(Pos(row, col + 1), time + 1),
      State(Pos(row, col - 1), time + 1)
    ).filter(inBounds(maxRow, maxCol, start, goal))
    .filterNot(blizzards.contains)

  private def inBounds(maxRow: Int, maxCol: Int, start: Pos, goal: Pos)(state: State): Boolean =
    val State(Pos(row, col), _) = state
    (row > 0 && row < maxRow && col > 0 && col < maxCol) || state.pos == start || state.pos == goal

  class Blizzards private (width: Int, height: Int, movingRight: Map[Int, Set[Int]], movingLeft: Map[Int, Set[Int]], movingUp: Map[Int, Set[Int]], movingDown: Map[Int, Set[Int]]):
    def contains(state: State): Boolean =
      val State(Pos(row, col), time) = state
      val colOffset = time % width
      val rowOffset = time % height
      val right = (col - colOffset + width - 1) % width + 1
      val left  = (col + colOffset - 1) % width + 1
      val down  = (row - rowOffset + height - 1) % height + 1
      val up    = (row + rowOffset - 1) % height + 1
      movingRight.getOrElse(row, Set.empty).contains(right) ||
      movingLeft.getOrElse(row, Set.empty).contains(left) ||
      movingUp.getOrElse(col, Set.empty).contains(up) ||
      movingDown.getOrElse(col, Set.empty).contains(down)

  object Blizzards:
    def fromGrid(grid: Grid[Char]): Blizzards =
      val width  = grid.keys.map(_.col).max - 1
      val height = grid.keys.map(_.row).max - 1
      val movingRight = grid.filter(_._2 == '>').map(_._1).groupMap(_.row)(_.col).view.mapValues(_.toSet).toMap
      val movingLeft  = grid.filter(_._2 == '<').map(_._1).groupMap(_.row)(_.col).view.mapValues(_.toSet).toMap
      val movingUp    = grid.filter(_._2 == '^').map(_._1).groupMap(_.col)(_.row).view.mapValues(_.toSet).toMap
      val movingDown  = grid.filter(_._2 == 'v').map(_._1).groupMap(_.col)(_.row).view.mapValues(_.toSet).toMap
      new Blizzards(width, height, movingRight, movingLeft, movingUp, movingDown)
