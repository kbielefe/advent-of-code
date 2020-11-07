package advent2018
import common.{Day, Grid}
import scala.io.Source
import monix.eval.Coeval

class Day13(source: Source) extends Day {
  sealed abstract class Cell(char: Char) {
    def move(trackGrid: Grid[Cell], cartGrid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = (cartGrid, None)
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

    override def move(trackGrid: Grid[Cell], cartGrid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = {
      val (x, y) = coords
      val (xOffset, yOffset) = moveForward
      val newCoords@(newX, newY) = (x + xOffset, y + yOffset)
      val cellAtDestOption = trackGrid.getCell(newX, newY)
      val cellAtDest = cellAtDestOption.get
      val turnedCart: Cart = cellAtDest match {
        case Intersection() => turnAtIntersection
        case t: TurnTrack   => turn(t)
        case _              => this
      }
      val gridAfterMove = cartGrid.move(coords, newCoords)
      val gridAfterTurn = gridAfterMove.replace(newCoords, turnedCart)
      val stack = gridAfterTurn.getStack(newX, newY)
      val crash = if (stack.size > 1) Some(newCoords) else None
      val gridAfterRemovingCrashes = if (crash.isDefined) gridAfterTurn.delete(newCoords).delete(newCoords).delete(newCoords).delete(newCoords) else gridAfterTurn
      (gridAfterRemovingCrashes, crash)
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

  def isCart(cell: Cell): Boolean = cell match {
    case c: Cart => true
    case _       => false
  }

  def isTrack(cell: Cell): Boolean = cell match {
    case c: Track => true
    case _        => false
  }

  // Output is optional crash location
  def turn(trackGrid: Grid[Cell])(cartGrid: Grid[Cell], coords: (Int, Int)): (Grid[Cell], Option[(Int, Int)]) = {
    val cell = cartGrid.getCell(coords._1, coords._2)
    cell.map{_.move(trackGrid, cartGrid, coords)}.getOrElse((cartGrid, None))
  }

  lazy val grid = parseGrid(source)
  lazy val cartGrid = grid filter isCart
  lazy val trackGrid = grid filter isTrack

  def firstCrash: (Int, Int) = {
    cartGrid.turns[Coeval, Option[(Int, Int)], (Int, Int)](turn(trackGrid), _.readingOrder(isCart))
      .map{_._2}
      .findL{_.isDefined}
      .value().get.get
  }

  def lastManStanding: (Int, Int) = {
    val coords = cartGrid.turns[Coeval, Option[(Int, Int)], (Int, Int)](turn(trackGrid), _.readingOrder(isCart))
      .map{_._1}
      .dropWhile{_.size > 1}
      .drop(1)
      .headOptionL
      .value().get.getAllCoords

    coords.head
  }


  override def answer1: String = firstCrash.toString
  // 74,87 is correct
  override def answer2: String = lastManStanding.toString
  // 29,74 is correct
}
