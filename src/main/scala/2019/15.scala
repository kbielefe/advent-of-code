package advent2019
import common.{AStar, DayTask, Intcode}
import monix.eval.Task
import monix.reactive.Observable
import cats.effect.concurrent.{MVar, Ref}

class Day15 extends DayTask[Map[Long, Long], Int, String] {
  override def input(lines: Observable[String]) = lines.headL.map{line =>
    line.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap
  }

  val North = 1
  val South = 2
  val West  = 3
  val East  = 4

  val Blocked      = 0
  val Moved        = 1
  val FoundOxygen  = 2

  sealed trait Square
  case object Wall       extends Square
  case object Empty      extends Square
  case object Oxygen     extends Square
  case object Unexplored extends Square
  case object Origin     extends Square

  def heuristic(from: (Int, Int), to: (Int, Int)): Double = {
    math.abs(from._1 - to._1).toDouble + math.abs(from._2 - to._2).toDouble
  }

  def edgeWeight(from: (Int, Int), to: (Int, Int)): Double = 1.0

  def getNeighbors(map: Map[(Int, Int), Square])(from: (Int, Int)): Set[(Int, Int)] = {
    val (x, y) = from
    val allNeighbors = Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
    allNeighbors.filter{neighbor => map.getOrElse(neighbor, Empty) != Wall}
  }

  def getPassableNeighbors(map: Map[(Int, Int), Square])(from: (Int, Int)): Set[(Int, Int)] = {
    val (x, y) = from
    val allNeighbors = Set((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
    allNeighbors.filter{neighbor => map.getOrElse(neighbor, Wall) != Wall}
  }

  def explore(position: (Int, Int), map: Map[(Int, Int), Square]): Long = {
    val astar = new AStar(heuristic, edgeWeight, getNeighbors(map))
    val path = astar.getPath(position, (-16, -12))
    val (x, y) = position
    val (pathX, pathY) = path.drop(1).head
    val dx = pathX - x
    val dy = pathY - y
    (dx, dy) match {
      case (1, 0)  => East
      case (-1, 0) => West
      case (0, 1)  => North
      case (0, -1) => South
      case _       => throw new Exception("Invalid path")
    }
  }

  def calculateDistance(map: Map[(Int, Int), Square]): Int = {
    val astar = new AStar(heuristic, edgeWeight, getPassableNeighbors(map))
    astar.getPath((0, 0), (-16, -12)).size - 1
  }

  def draw(map: Map[(Int, Int), Square]): Unit = {
    val withOrigin = map.updated((0, 0), Origin).updated((-16, -12), Oxygen)
    val minX = map.keys.map(_._1).min
    val maxX = map.keys.map(_._1).max
    val minY = map.keys.map(_._2).min
    val maxY = map.keys.map(_._2).max
    val grid = (maxY to minY by -1).map{y =>
      (minX to maxX).map{x =>
        withOrigin.getOrElse((x, y), Unexplored) match {
          case Wall       => '#'
          case Empty      => '.'
          case Oxygen     => 'O'
          case Origin     => 'I'
          case Unexplored => ' '
        }
      }.mkString
    }.mkString("\n")
    println(grid)
  }

  def updatePosition(position: (Int, Int), command: Long, status: Long): (Int, Int) = {
    val (x, y) = position
    if (status == Blocked) {
      position
    } else command match {
      case North => (x, y + 1)
      case South => (x, y - 1)
      case East  => (x + 1, y)
      case West  => (x - 1, y)
    }
  }

  def updateMap(position: (Int, Int), command: Long, status: Long, map: Map[(Int, Int), Square]): Map[(Int, Int), Square] = {
    val positionToUpdate = updatePosition(position, command, Moved)
    val square = status match {
      case Blocked     => Wall
      case Moved       => Empty
      case FoundOxygen => Oxygen
    }
    map.updated(positionToUpdate, square)
  }

  def controller(commands: MVar[Task, Long], statuses: MVar[Task, Long], position: (Int, Int) = (0, 0), map: Map[(Int, Int), Square] = Map.empty): Task[Int] = for {
    command <- Task{explore(position, map)}
    _       <- commands.put(command)
    status  <- statuses.take
    newPos  <- Task{updatePosition(position, command, status)}
    newMap  <- Task{updateMap(position, command, status, map)}
    result  <- if (status == FoundOxygen) Task{calculateDistance(newMap)} else controller(commands, statuses, newPos, newMap)
  } yield result

  def run(initialMemory: Map[Long, Long]) = for {
    in      <- MVar.empty[Task, Long]
    out     <- MVar.empty[Task, Long]
    program <- new Intcode("repair", in, out).run(initialMemory, 0, 0).start
    result  <- controller(in, out)
    _       <- program.cancel
  } yield result

  // 45 and 46 are wrong
  override def part1(initialMemory: Map[Long, Long]) = run(initialMemory)

  override def part2(initialMemory: Map[Long, Long]) = Task{"unimplemented"}
}
