package advent2018
import common.{AStar, DayTask}
import monix.eval.Task
import monix.reactive.Observable

class Day22 extends DayTask[(Int, (Int, Int)), Int, Int] {

  override def input(lines: Observable[String]) = Task{(10914, (9, 739))}
  //override def input(lines: Observable[String]) = Task{(510, (10, 10))}

  val Rocky  = 0
  val Wet    = 1
  val Narrow = 2

  def geologicIndex(above: Int, left: Int, x: Int, y: Int, TargetX: Int, TargetY: Int): Int = (x, y) match {
    case (0, 0)             => 0
    case (TargetX, TargetY) => 0
    case (x, 0)             => x * 16807
    case (0, y)             => y * 48271
    case (x, y)             => above * left
  }

  def erosion(depth: Int, targetX: Int, targetY: Int, maxX: Int, maxY: Int): Seq[Seq[Int]] = {
    val initialAbove = Seq.fill(maxX + 1)(1)
    (0 to maxY).scanLeft(initialAbove){case (rowAbove, y) =>
      (0 to maxX).scanLeft(1){case (left, x) =>
        val above = rowAbove(x)
        (geologicIndex(above, left, x, y, targetX, targetY) + depth) % 20183
      }.drop(1)
    }.drop(1)
  }

  def regionTypes(erosion: Seq[Seq[Int]]): Seq[Seq[Int]] =
    erosion.map(_.map(_ % 3))

  def getRegionsMap(regions: Seq[Seq[Int]]): Map[(Int, Int), Int] = {
    regions.zipWithIndex.flatMap{case (row, y) =>
      row.zipWithIndex.map{case (region, x) =>
        ((x, y), region)
      }
    }.toMap
  }

  override def part1(input: (Int, (Int, Int))) = Task{
    val (depth, (targetX, targetY)) = input
    regionTypes(erosion(depth, targetX, targetY, targetX, targetY)).map(_.sum).sum
  }

  type Position = ((Int, Int), Tool)

  sealed trait Tool
  case object ClimbingGear extends Tool
  case object Torch        extends Tool
  case object Neither      extends Tool

  def heuristic(from: Position, to: Position): Double = {
    val toolSwitchWeight = if (from._2 != to._2) 7.0 else 0.0
    val manhattanDistance =
      math.abs(from._1._1 - to._1._1) + math.abs(from._1._2 - to._1._2)
    toolSwitchWeight + manhattanDistance.toDouble
  }

  def edgeWeight(from: Position, to: Position): Double = {
    val toolSwitchWeight = if (from._2 != to._2) 7.0 else 0.0
    val movementWeight   = if (from._1 != to._1) 1.0 else 0.0
    toolSwitchWeight + movementWeight
  }

  def getNeighbors(regions: Map[(Int, Int), Int], maxX: Int, maxY: Int)(current: Position): Set[Position] = {
    val (position, tool) = current
    val switchedTool = (regions(position), tool) match {
      case (Rocky,  ClimbingGear) => Torch
      case (Rocky,  Torch)        => ClimbingGear
      case (Wet,    ClimbingGear) => Neither
      case (Wet,    Neither)      => ClimbingGear
      case (Narrow, Torch)        => Neither
      case (Narrow, Neither)      => Torch
      case _                      => throw new Exception(s"Invalid current position: $current")
    }
    val (x, y) = position
    val moves = Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
    val inBounds = moves.filter{case (x, y) => x >= 0 && y >= 0 && x <= maxX && y <= maxY}
    val valid = inBounds.filter(position => validRegionForTool(tool, regions(position)))
    valid.map(position => (position, tool)) + ((position, switchedTool))
  }

  def validRegionForTool(tool: Tool, region: Int): Boolean = region match {
    case Rocky  => tool == ClimbingGear || tool == Torch
    case Wet    => tool == ClimbingGear || tool == Neither
    case Narrow => tool == Torch || tool == Neither
  }

  def totalMinutes(path: List[Position]): Int = {
    path.sliding(2).map{case List(p1, p2) =>
      if (p1._2 != p2._2) 7 else 1
    }.sum
  }

  def draw(regionsMap: Map[(Int, Int), Int], targetX: Int, targetY: Int, maxX: Int, maxY: Int): Unit = {
    val targetChar = regionsMap((targetX, targetY)) match {
      case Rocky  => 'R'
      case Wet    => 'W'
      case Narrow => 'N'
    }
    val mapWithChars = regionsMap.mapValues{_ match {
      case Rocky  => '.'
      case Wet    => '='
      case Narrow => '|'
    }}
    val mapWithTarget = mapWithChars.updated((targetX, targetY), targetChar)
    val map = (0 to maxY).map{y =>
      (0 to maxX).map{x => mapWithTarget((x, y))
      }.mkString
    }.mkString("\n")
    println(map)
  }

  override def part2(input: (Int, (Int, Int))) = Task{
    val (depth, (targetX, targetY)) = input
    val maxX = targetX + 40
    val maxY = targetY + 10
    val regions = regionTypes(erosion(depth, targetX, targetY, maxX, maxY))
    val regionsMap = getRegionsMap(regions)
    val aStar = new AStar(heuristic, edgeWeight, getNeighbors(regionsMap, maxX, maxY))
    val start = ((0, 0), Torch)
    val goal  = ((targetX, targetY), Torch)
    val path = aStar.getPath(start, goal)
    totalMinutes(path)
  }
  // 1052 is too high
}
