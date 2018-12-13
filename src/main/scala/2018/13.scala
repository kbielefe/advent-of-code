package advent2018
import common.{Day, Visualize}
import scala.io.Source

class Day13(source: Source) extends Day {
  val input = source

  type Path  = Map[(Int, Int), Char]
  type Carts = Map[(Int, Int), Cart]

  case class Cart(facing: Char, lastTurn: Char)

  // TODO: Make a generic parseGrid function from Source or String
  // Allows different coordinate systems: row/col, x/y, etc.
  def parsePath(in: Source): Path = {
    def parseRow(path: Path, row: Int, line: String): Path = {
      line.foldLeft((path, 0)){case ((path, col), char) => (path + ((col, row) -> char), col + 1)}._1
    }
    in.getLines.foldLeft((Map.empty[(Int, Int), Char], 0)){case ((path, row), line) => (parseRow(path, row, line), row + 1)}._1
  }

  def getCarts(path: Path): Carts = {
    path
      .filter{"^v<>" contains _._2}
      .map{case ((x, y), char) => ((x, y) -> Cart(char, 'R'))}
      .toMap
  }

  def removeCartsFromPath(path: Path): Path = {
    path.map{
      case (c, '^') => (c, '|')
      case (c, 'v') => (c, '|')
      case (c, '<') => (c, '-')
      case (c, '>') => (c, '-')
      case x        => x
    }
  }

  def move(path: Char, cart: Cart): (Int, Int, Cart) = (path, cart) match {
    case ('/', Cart('^', t)) => ( 1,  0, Cart('>', t))
    case ('/', Cart('v', t)) => (-1,  0, Cart('<', t))
    case ('/', Cart('>', t)) => ( 0, -1, Cart('^', t))
    case ('/', Cart('<', t)) => ( 0,  1, Cart('v', t))

    case ('\\', Cart('^', t)) => (-1,  0, Cart('<', t))
    case ('\\', Cart('v', t)) => ( 1,  0, Cart('>', t))
    case ('\\', Cart('>', t)) => ( 0,  1, Cart('v', t))
    case ('\\', Cart('<', t)) => ( 0, -1, Cart('^', t))

    case ('-', Cart('<', t)) => (-1, 0, Cart('<', t))
    case ('-', Cart('>', t)) => ( 1, 0, Cart('>', t))

    case ('|', Cart('^', t)) => (0, -1, Cart('^', t))
    case ('|', Cart('v', t)) => (0,  1, Cart('v', t))

    case ('+', Cart('v', 'L')) => ( 0, 1, Cart('v', 'S'))
    case ('+', Cart('v', 'S')) => (-1, 0, Cart('<', 'R'))
    case ('+', Cart('v', 'R')) => ( 1, 0, Cart('>', 'L'))

    case ('+', Cart('^', 'L')) => ( 0, -1, Cart('^', 'S'))
    case ('+', Cart('^', 'S')) => ( 1,  0, Cart('>', 'R'))
    case ('+', Cart('^', 'R')) => (-1,  0, Cart('<', 'L'))

    case ('+', Cart('<', 'L')) => (-1,  0, Cart('<', 'S'))
    case ('+', Cart('<', 'S')) => ( 0, -1, Cart('^', 'R'))
    case ('+', Cart('<', 'R')) => ( 0,  1, Cart('v', 'L'))

    case ('+', Cart('>', 'L')) => ( 1,  0, Cart('>', 'S'))
    case ('+', Cart('>', 'S')) => ( 0,  1, Cart('v', 'R'))
    case ('+', Cart('>', 'R')) => ( 0, -1, Cart('^', 'L'))
  }

  def moveCarts(path: Path, carts: Carts): Carts = {
    carts.map{case ((x, y), cart) =>
      val (xOffset, yOffset, newCart) = move(path(x, y), cart)
      ((x + xOffset, y + yOffset), newCart)
    }
  }

  def firstCrash(path: Path, carts: Carts): (Int, Int) = {
    def crashed(carts: Carts): Set[(Int, Int)] = {
      carts.groupBy{_._1}.mapValues{_.size}.filter{_._2 > 1}.map{_._1}.toSet
    }
    val animation = Iterator.iterate(carts)(moveCarts(path, _)).take(20)
    animation.map(crashed).find(!_.isEmpty).get.head
  }

  def pathToString(path: Path, carts: Carts): Iterator[String] = {
    val combined: Path = path ++ (carts mapValues {_.facing})
    Visualize.gridToString{case (x, y) => combined.getOrElse((x, y), ' ')}(0, 0, combined.map{_._1._1}.max + 1, combined.map{_._1._2}.max + 1)
  }

  def printAnimation(path: Path, carts: Carts): Unit = {
    val animation = Iterator.iterate(carts)(moveCarts(path, _)).take(20)
    animation.map(pathToString(path, _)) foreach {lines =>
      lines foreach println
      println("")
    }
  }

  override def answer1: String = ???
  override def answer2: String = ???
}
