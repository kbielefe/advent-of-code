package advent2018
import common.{Day, Grid}
import scala.io.Source

class Day13(source: Source) extends Day {
  sealed abstract class Cell(gridChar: Char) extends Grid.Cell {
    override def char = gridChar
    def move(grid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = (grid, None)
  }

  abstract class Track(gridChar: Char) extends Cell(gridChar)
  abstract class Cart(gridChar: Char)  extends Cell(gridChar) {
    def move(grid: Grid[Cell]) = {
      ???
    }
  }

  case class Intersection()   extends Track('+')
  case class HorizontalTrack() extends Track('-')
  case class VerticalTrack()   extends Track('|')
  case class SlashTrack()      extends Track('/')
  case class BackslashTrack()  extends Track('\\')

  case class UpCart()    extends Cart('^')
  case class DownCart()  extends Cart('v')
  case class LeftCart()  extends Cart('<')
  case class RightCart() extends Cart('>')

  def charToCell(char: Char): Cell = char match {
    case '^'  => UpCart()
    case 'v'  => DownCart()
    case '<'  => LeftCart()
    case '>'  => RightCart()
    case '+'  => Intersection()
    case '-'  => HorizontalTrack()
    case '|'  => VerticalTrack()
    case '/'  => SlashTrack()
    case '\\' => BackslashTrack()
  }

  def under(char: Char): Option[Char] = char match {
    case '^'  => Some('|')
    case 'v'  => Some('|')
    case '<'  => Some('-')
    case '>'  => Some('-')
    case _    => None
  }

  def parseGrid(input: Source): Grid[Cell] = Grid(0, 0, ' ', input, under _, charToCell _)

  def order(grid: Grid[Cell]): Iterator[(Int, Int)] =
    grid.readingOrder{
      case c: Cart => true
      case _       => false
    }

  // Output is optional crash location
  def turn(grid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = {
    val cell = grid.getCell(coords._1, coords._2)
    cell.map{_.move(grid, coords)}.getOrElse((grid, None))
  }

  def firstCrash(input: Source): (Int, Int) = {
    val grid = parseGrid(input)
    grid.rounds(turn, order).map{_._2}.find{_.isDefined}.get.get
  }

  def lastManStanding(input: Source): (Int, Int) = ???

  override def answer1: String = ???
  // 74,87 is correct
  override def answer2: String = ???
  // 24, 57 is wrong
  // 148, 109 is wrong
  // 149, 109 is wrong
  // 147, 109 is wrong
}
