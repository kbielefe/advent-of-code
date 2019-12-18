package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

class Day18 extends DayTask[Day18.Maze, Int, String] {

  import Day18._

  // TODO: Make a common routine for doing this
  override def input(lines: Observable[String]) = lines.zipWithIndex.flatMap{case (line, y) =>
    Observable.fromIterable(line.zipWithIndex.map{case (char, x) =>
      val square = char match {
        case '#' => Wall
        case '.' => Passage
        case '@' => Entrance
        case k if k.isLower => Key(k)
        case d   => Door(d)
      }
      (x, y.toInt) -> square
    })
  }.toListL.map(_.toMap)

  type DAG = Map[Key, Key]

  def getNeighbors(maze: Maze, position: Position): Set[Position] = {
    val (x, y) = position
    Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)) filter {position =>
      maze(position) match {
        case Wall => false
        case _    => true
      }
    }
  }

  def createDAG(maze: Maze, position: Position, visited: Set[Position] = Set.empty, previousDoors: Set[Door] = Set.empty, dag: DAG = Map.empty): DAG = {
    val neighbors = getNeighbors(maze, position) -- visited
    if (neighbors.isEmpty) {
      dag
    } else {
      neighbors.foldLeft(dag){case (dag, neighbor) =>
        createDAG(maze, neighbor, visited + position, previousDoors, dag)
      }
    }
  }

  override def part1(maze: Maze) = Task{0}

  override def part2(maze: Maze) = Task{"unimplemented"}
}

object Day18 {
  sealed trait Square
  case object Wall             extends Square
  case object Entrance         extends Square
  case object Passage          extends Square
  case class  Key(name: Char)  extends Square
  case class  Door(name: Char) extends Square

  type Position = (Int, Int)

  type Maze = Map[Position, Square]
}
