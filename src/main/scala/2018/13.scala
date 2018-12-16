package advent2018
import common.{Day, Grid}
import scala.io.Source
import monix.eval.Coeval

class Day13(source: Source) extends Day {
  sealed abstract class Cell(gridChar: Char) extends Grid.Cell {
    override def char = gridChar
    def move(grid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = (grid, None)
  }

  abstract class Track(gridChar: Char) extends Cell(gridChar)
  abstract class TurnTrack(gridChar: Char) extends Track(gridChar)
  abstract class Cart(gridChar: Char, lastTurn: Char = 'R') extends Cell(gridChar) {
    def moveForward: (Int, Int)
    def turn(track: TurnTrack): Cart
    def turnLeft: Cart
    def turnRight: Cart
    def setLastTurn(newLastTurn: Char): Cart

    def turnAtIntersection: Cart = {
      val nextTurn = lastTurn match {
        case 'L' => 'S'
        case 'S' => 'R'
        case 'R' => 'L'
      }
      val turnedCart = nextTurn match {
        case 'L' => turnLeft
        case 'S' => this
        case 'R' => turnRight
      }
      turnedCart.setLastTurn(nextTurn)
    }

    override def move(grid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = {
      val (x, y) = coords
      val (xOffset, yOffset) = moveForward
      val newCoords@(newX, newY) = (x + xOffset, y + yOffset)
      val cellAtDestOption = grid.getCell(newX, newY)
      val cellAtDest = cellAtDestOption.get
      val turnedCart: Cart = cellAtDest match {
        case Intersection() => turnAtIntersection
        case t: TurnTrack   => turn(t)
        case _              => this
      }
      val gridAfterMove = grid.move(coords, newCoords)
      val gridAfterTurn = gridAfterMove.replace(newCoords, turnedCart)
      val stack = gridAfterTurn.getStack(newX, newY)
      val crash = if (stack.size > 2) Some(newCoords) else None
      (gridAfterTurn, crash)
    }
  }

  case class Intersection()    extends Track('+')
  case class HorizontalTrack() extends Track('-')
  case class VerticalTrack()   extends Track('|')
  case class SlashTrack()      extends TurnTrack('/')
  case class BackslashTrack()  extends TurnTrack('\\')

  case class UpCart(lastTurn: Char = 'R') extends Cart('^', lastTurn) {
    override def moveForward = (0, -1)
    override def turn(track: TurnTrack) = track match {
      case SlashTrack()      => turnRight
      case BackslashTrack()  => turnLeft
    }
    override def turnLeft  = LeftCart(lastTurn)
    override def turnRight = RightCart(lastTurn)
    override def setLastTurn(newLastTurn: Char) = UpCart(newLastTurn)
  }
  
  case class DownCart(lastTurn: Char = 'R')  extends Cart('v', lastTurn) {
    override def moveForward = (0, 1)
    override def turn(track: TurnTrack) = track match {
      case SlashTrack()      => turnRight
      case BackslashTrack()  => turnLeft
    }
    override def turnLeft  = RightCart(lastTurn)
    override def turnRight = LeftCart(lastTurn)
    override def setLastTurn(newLastTurn: Char) = DownCart(newLastTurn)
  }

  case class LeftCart(lastTurn: Char = 'R')  extends Cart('<', lastTurn) {
    override def moveForward = (-1, 0)
    override def turn(track: TurnTrack) = track match {
      case SlashTrack()      => turnLeft
      case BackslashTrack()  => turnRight
    }
    override def turnLeft  = DownCart(lastTurn)
    override def turnRight = UpCart(lastTurn)
    override def setLastTurn(newLastTurn: Char) = LeftCart(newLastTurn)
  }

  case class RightCart(lastTurn: Char = 'R') extends Cart('>', lastTurn) {
    override def moveForward = (1, 0)
    override def turn(track: TurnTrack) = track match {
      case SlashTrack()      => turnLeft
      case BackslashTrack()  => turnRight
    }
    override def turnLeft  = UpCart(lastTurn)
    override def turnRight = DownCart(lastTurn)
    override def setLastTurn(newLastTurn: Char) = RightCart(newLastTurn)
  }

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

  def order(grid: Grid[Cell]): List[(Int, Int)] =
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
    grid.rounds[Coeval, Option[(Int, Int)]](turn, order)
      .map{_._2}
      .findL{_.isDefined}
      .value.get.get
  }

  def lastManStanding(input: Source): (Int, Int) = ???

  override def answer1: String = firstCrash(source).toString
  // 74,87 is correct
  override def answer2: String = ???
  // 24, 57 is wrong
  // 148, 109 is wrong
  // 149, 109 is wrong
  // 147, 109 is wrong
}
