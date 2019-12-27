package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

class Day20 extends DayTask[Day20.Maze, Int, String] {

  import Day20._

  override def input(lines: Observable[String]) = lines.zipWithIndex.flatMap{case (line, y) =>
    Observable.fromIterable(line.zipWithIndex.flatMap{case (char, x) =>
      val square = char match {
        case '#' => None
        case ' ' => None
        case '.' => Some(Passage)
        case x   => Some(Label(x))
      }
      square.map((x, y.toInt) -> _)
    })
  }.toListL.map(_.toMap)

  def findPortals(maze: Maze): Maze = {
    val labels = maze.filter{
      case (_, Label(_)) => true
      case _             => false
    }
    val passages = maze.filter{
      case (_, Passage) => true
      case _            => false
    }
    val portals = labels.flatMap{
      case ((x, y), Label(char)) if labels.contains((x, y+1)) && passages.contains((x, y+2)) => Some((x, y + 2) -> Portal(s"$char${labels((x, y+1))}"))
      case ((x, y), Label(char)) if labels.contains((x, y+1)) && passages.contains((x, y-1)) => Some((x, y - 1) -> Portal(s"$char${labels((x, y+1))}"))
      case ((x, y), Label(char)) if labels.contains((x+1, y)) && passages.contains((x+2, y)) => Some((x + 2, y) -> Portal(s"$char${labels((x+1, y))}"))
      case ((x, y), Label(char)) if labels.contains((x+1, y)) && passages.contains((x-1, y)) => Some((x - 1, y) -> Portal(s"$char${labels((x+1, y))}"))
      case _ => None
    }
    maze ++ portals
  }

  override def part1(maze: Maze) = Task{0}

  override def part2(maze: Maze) = Task{"unimplemented"}
}

object Day20 {
  sealed trait Square
  case object Passage              extends Square
  case class  Label(char: Char)    extends Square
  case class  Portal(name: String) extends Square

  type Position = (Int, Int)

  type Maze = Map[Position, Square]
}
