package advent2018
import common.{Day, Visualize}
import scala.io.Source

class Day13(source: Source) extends Day {
  val input = source

  type Path    = Map[(Int, Int), Char]
  type Carts   = Map[(Int, Int, Int), Cart]
  type Crashes = Set[(Int, Int)]

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
      .zipWithIndex
      .map{case (((x, y), char), z) => ((x, y, z) -> Cart(char, 'R'))}
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

  def moveCarts(path: Path, carts: Carts): (Carts, Crashes) = {
    val sorted = carts.toList.sortBy(_._1)
    sorted.foldLeft((carts, Set.empty[(Int, Int)])){case ((oldCarts, crashes), ((x, y, z), cart)) =>
      val (xOffset, yOffset, newCart) = move(path(x, y), cart)
      val newX = x + xOffset
      val newY = y + yOffset
      val newCarts = oldCarts - ((x, y, z)) + (((newX, newY, z), newCart))
      val crashed = oldCarts.exists{case ((oldX, oldY, _), _) => oldX == newX && oldY == newY}
      val newCrashes = if (crashed) crashes + ((newX, newY)) else crashes
      (newCarts, newCrashes)
    }
  }

  def animation(path: Path, carts: Carts): Iterator[(Carts, Crashes)] = {
    Iterator.iterate((carts, Set.empty[(Int, Int)])){case (oldCarts, crashes) =>
      val (newCarts, newCrashes) = moveCarts(path, oldCarts)
      (newCarts, crashes ++ newCrashes)
    }
  }

  // TODO: Look up auto-currying in Scala
  // TODO: Make a runner option to step through an animation
  def firstCrash(path: Path, carts: Carts): (Int, Int) = {
    animation(path, carts).find(!_._2.isEmpty).get._2.head
  }

  def pathToString(path: Path, carts: Carts): Iterator[String] = {
    val combined: Path = path ++ (carts map {case ((x, y, z), cart) => ((x, y), cart.facing)})
    Visualize.gridToString{case (x, y) => combined.getOrElse((x, y), ' ')}(0, 0, combined.map{_._1._1}.max + 1, combined.map{_._1._2}.max + 1)
  }

  def firstCrash(input: Source): (Int, Int) = {
    val rawPath = parsePath(input)
    val path  = removeCartsFromPath(rawPath)
    val carts = getCarts(rawPath)
    firstCrash(path, carts)
  }

  override def answer1: String = firstCrash(input).toString
  override def answer2: String = ???
}
