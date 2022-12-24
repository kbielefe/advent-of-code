package advent2022
import puzzleparse.Pos
import scala.annotation.tailrec

object Day22:
  def part1(input: String): Int = answer(input, wrapPart1)
  def part2(input: String): Int = answer(input, wrapPart2)

  def answer(input: String, wrap: Wrap): Int =
    val Array(gridString, moveString) = input.split("\n\n")
    val grid = parseGrid(gridString)
    val startingPos = grid.map(_._1).filter(_.row == 1).minBy(_.col)
    val startingDir = '>'
    import Move.*
    val (Pos(endRow, endCol), facing) = moves(moveString.trim).foldLeft((startingPos, startingDir)){
      case ((pos, dir), Forward(distance)) => moveForward(pos, dir, distance, grid, wrap)
      case ((pos, dir), Turn('L')) => dir match
        case '>' => (pos, '^')
        case '^' => (pos, '<')
        case '<' => (pos, 'v')
        case 'v' => (pos, '>')
      case ((pos, dir), Turn('R')) => dir match
        case '>' => (pos, 'v')
        case '^' => (pos, '>')
        case '<' => (pos, '^')
        case 'v' => (pos, '<')
    }
    1000 * endRow + 4 * endCol + ">v<^".indexOf(facing)

  @tailrec
  private def moveForward(pos: Pos, dir: Char, distance: Int, grid: Map[Pos, Char], wrap: Wrap): (Pos, Char) =
    if distance == 0 then
      (pos, dir)
    else
      val Pos(row, col) = pos
      val newPos = dir match
        case '>' => Pos(row, col + 1)
        case '^' => Pos(row - 1, col)
        case '<' => Pos(row, col - 1)
        case 'v' => Pos(row + 1, col)
      grid.get(newPos) match
        case Some('#') => (pos, dir)
        case Some('.') => moveForward(newPos, dir, distance - 1, grid, wrap)
        case None =>
          val (wrappedPos, wrappedDir) = wrap(pos, dir, grid)
          println(s"$pos $dir $wrappedPos $wrappedDir")
          if grid(wrappedPos) == '#' then
            (pos, dir)
          else
            moveForward(wrappedPos, wrappedDir, distance - 1, grid, wrap)

  type Wrap = (Pos, Char, Map[Pos, Char]) => (Pos, Char)

  private def wrapPart1(pos: Pos, dir: Char, grid: Map[Pos, Char]): (Pos, Char) =
    val Pos(row, col) = pos
    val newPos = dir match
      case '>' => grid.keySet.filter(_.row == row).minBy(_.col)
      case '<' => grid.keySet.filter(_.row == row).maxBy(_.col)
      case 'v' => grid.keySet.filter(_.col == col).minBy(_.row)
      case '^' => grid.keySet.filter(_.col == col).maxBy(_.row)
    (newPos, dir)

  private def wrapPart2(pos: Pos, dir: Char, grid: Map[Pos, Char]): (Pos, Char) =
    val Pos(row, col) = pos
    val face = (row - 1) / 50 -> (col - 1) / 50
    val offset = dir match
      case '>' => (row - 1) % 50
      case '<' => (row - 1) % 50
      case 'v' => (col - 1) % 50
      case '^' => (col - 1) % 50
    println(s"$pos $dir $face $offset")
    val (newFace, newDir) = (face, dir) match
      case ((0, 1), '<') => ((2, 0), '>')
      case ((0, 1), '^') => ((3, 0), '>')
      case ((0, 2), '^') => ((3, 0), '^')
      case ((0, 2), 'v') => ((1, 1), '<')
      case ((0, 2), '>') => ((2, 1), '<')
      case ((1, 1), '<') => ((2, 0), 'v')
      case ((1, 1), '>') => ((0, 2), '^')
      case ((2, 0), '^') => ((1, 1), '>')
      case ((2, 0), '<') => ((0, 1), '>')
      case ((2, 1), '>') => ((0, 2), '<')
      case ((2, 1), 'v') => ((3, 0), '<')
      case ((3, 0), '<') => ((0, 1), 'v')
      case ((3, 0), 'v') => ((0, 2), 'v')
      case ((3, 0), '>') => ((2, 1), '^')
      case _ => ???
    val flippedOffset =
      if newDir == '>' && dir == '<' || newDir == '<' && dir == '>' then
        50 - offset
      else
        offset
    val newPos = newDir match
      case '>' => Pos(newFace._1 * 50 + flippedOffset + 1, newFace._2 * 50 + 1)
      case '<' => Pos(newFace._1 * 50 + flippedOffset + 1, newFace._2 * 50 + 50)
      case 'v' => Pos(newFace._1 * 50 +  1, newFace._2 * 50 + flippedOffset + 1)
      case '^' => Pos(newFace._1 * 50 + 50, newFace._2 * 50 + flippedOffset + 1)
    //println(s"$face $offset $newFace $flippedOffset")
    (newPos, newDir)

  private def parseGrid(input: String): Map[Pos, Char] =
    input.linesIterator.filterNot(_.isEmpty).zipWithIndex.flatMap{(line, row) =>
      line.zipWithIndex.map{(elem, col) => (Pos(row + 1, col + 1), elem)}
    }.toMap.filterNot(_._2 == ' ')

  enum Move:
    case Forward(distance: Int)
    case Turn(direction: Char)

  private def moves(input: String): Iterator[Move] =
    if input.isEmpty then
      Iterator.empty
    else if input.head.isDigit then
      val (distance, rest) = input.span(_.isDigit)
      Iterator(Move.Forward(distance.toInt)) ++ moves(rest)
    else
      Iterator(Move.Turn(input.head)) ++ moves(input.tail)
