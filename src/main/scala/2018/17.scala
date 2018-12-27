package advent2018
import common.{Day, Grid}
import scala.io.Source

class Day17(source: Source) extends Day {

  sealed abstract class Cell
  case class Clay() extends Cell
  abstract class Water extends Cell
  case class RunningWater() extends Water
  case class StandingWater() extends Water

  val xRangeRegex = """y=(\d+), x=(\d+)..(\d+)""".r
  val yRangeRegex = """x=(\d+), y=(\d+)..(\d+)""".r

  def parseInput(source: Source): Grid[Cell] = {
    def parseLine(line: String): Seq[((Int, Int), List[Cell])] = line match {
      case xRangeRegex(y, xMin, xMax) => (xMin.toInt to xMax.toInt) map {x => ((x, y.toInt), List(Clay()))}
      case yRangeRegex(x, yMin, yMax) => (yMin.toInt to yMax.toInt) map {y => ((x.toInt, y), List(Clay()))}
    }
    val zorders = source.getLines.flatMap(parseLine).toMap
    new Grid(zorders)
  }

  lazy val grid = parseInput(source)

  def cellToChar(cell: Cell): Char = cell match {
    case Clay()          => '#'
    case RunningWater()  => '|'
    case StandingWater() => '~'
  }

  def printGrid(grid: Grid[Cell]): Unit = grid.getLines('.', cellToChar, userLeft = Some(450), userRight = Some(550)) foreach println
  printGrid(grid)

  override def answer1: String = ???
  override def answer2: String = ???
}
