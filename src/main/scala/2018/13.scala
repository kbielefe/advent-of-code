package advent2018
import common.{Day, Visualize}
import scala.io.Source
import cats.data.{State, IndexedStateT}
import cats.Applicative

class Day13(source: Source) extends Day {
  type Path    = Map[(Int, Int), Char]
  type Carts   = Map[(Int, Int, Int), Cart]
  type Crashes = Set[(Int, Int)]

  case class Cart(x: Int, y: Int, id: Int, facing: Char, lastTurn: Char)

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
      .map{case (((x, y), char), id) => ((x, y, id) -> Cart(x, y, id, char, 'R'))}
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

  def move(path: Char, cart: Cart): (Int, Int, Cart) = ???

  def moveCarts(path: Path, carts: Carts, removeCrashed: Boolean = false): (Carts, Crashes) = {
    val sorted = carts.toList.sortBy(_._1)
    sorted.foldLeft((carts, Set.empty[(Int, Int)])){case ((oldCarts, crashes), ((x, y, z), cart)) =>
      val (xOffset, yOffset, newCart) = move(path(x, y), cart)
      val newX = x + xOffset
      val newY = y + yOffset
      val movedCarts = oldCarts - ((x, y, z)) + (((newX, newY, z), newCart))
      val crashed = oldCarts.exists{case ((oldX, oldY, _), _) => oldX == newX && oldY == newY}
      val newCrashes = if (crashed) crashes + ((newX, newY)) else crashes
      val crashesRemoved = movedCarts.filterKeys{case (x, y, _) => !(x == newX && y == newY)}
      val newCarts = if (crashed && removeCrashed) crashesRemoved else movedCarts
      (newCarts, newCrashes)
    }
  }

  def animation(path: Path, carts: Carts, removeCrashed: Boolean = false): Iterator[(Carts, Crashes)] = {
    Iterator.iterate((carts, Set.empty[(Int, Int)])){case (oldCarts, crashes) =>
      val (newCarts, newCrashes) = moveCarts(path, oldCarts, removeCrashed)
      (newCarts, crashes ++ newCrashes)
    }
  }

  def crashTimes(path: Path, carts: Carts): Iterator[Int] = {
    animation(path, carts).zipWithIndex.filter{!_._1._2.isEmpty}.take(8622).map{_._2}
  }

  // TODO: Look up auto-currying in Scala
  // TODO: Make a runner option to step through an animation
  // TODO: Try to solve this using State monad
  def firstCrash(path: Path, carts: Carts): (Int, Int) = {
    animation(path, carts).find(!_._2.isEmpty).get._2.head
  }

  def lastManStanding(path: Path, carts: Carts): (Int, Int) = {
    //drawAnimation(path, carts, 100)
    val (x, y, _) = animation(path, carts, true).find(_._1.size <= 1).get._1.keys.head
    (x, y)
  }

  def firstCrash(input: Source): (Int, Int) = {
    val rawPath = parsePath(input)
    val path  = removeCartsFromPath(rawPath)
    val carts = getCarts(rawPath)
    firstCrash(path, carts)
  }

  def lastManStanding(input: Source): (Int, Int) = {
    val rawPath = parsePath(input)
    val path  = removeCartsFromPath(rawPath)
    val carts = getCarts(rawPath)
    lastManStanding(path, carts)
  }

  def drawAnimation(path: Path, carts: Carts, frames: Int): Unit = {
    //val visualizations = animation(path, carts, true).takeWhile(_._1.size > 1)
    val visualizations = animation(path, carts, true).drop(8620).take(5)
    val lines = for {
      (carts, crashes) <- visualizations
      line <- drawScreen(path, carts, crashes)
    } yield line
    lines foreach println
  }

  def drawScreen(path: Path, carts: Carts, crashes: Crashes): Iterator[String] = {
    val xs = carts.keySet.map{_._1}
    val ys = carts.keySet.map{_._2}
    val minX = xs.min - 5
    val minY = ys.min - 5
    val width = (xs.max - minX + 6)
    val height = (ys.max - minY + 6)
    val merged =
      path ++
      crashes.map{case (x, y) => ((x, y), 'X')}.toMap ++
      carts.map{case ((x, y, _), Cart(_, _, _, char, _)) => ((x, y), char)} ++
      Map(((148, 109), '*'))
    Visualize.gridToString{case (x, y) => merged.getOrElse((x, y), ' ')}(minX, minY, width, height) ++ Iterator("")
  }

  type CartState  = (Set[Cart], Cart)
  type UnitState  = State[CartState, Unit]
  type Collisions = Set[Cart]

  def mapCurrent(f: Cart => Cart): UnitState =
    State{case (carts, cart) => ((carts - cart + f(cart), f(cart)), ())}

  def moveForward: UnitState =
    mapCurrent{case Cart(x, y, id, facing, lastTurn) => 
      val (newX, newY) = facing match {
        case '^' => (x, y - 1)
        case 'v' => (x, y + 1)
        case '>' => (x + 1, y)
        case '<' => (x - 1, y)
      }
      Cart(newX, newY, id, facing, lastTurn)
    }

  def turnLeft(updateLast: Boolean = false): UnitState =
    mapCurrent{case Cart(x, y, id, facing, lastTurn) => 
      val newFacing = facing match {
        case '^' => '<'
        case 'v' => '>'
        case '>' => '^'
        case '<' => 'v'
      }
      Cart(x, y, id, newFacing, if (updateLast) 'L' else lastTurn)
    }

  def turnRight(updateLast: Boolean = false): UnitState =
    mapCurrent{case Cart(x, y, id, facing, lastTurn) => 
      val newFacing = facing match {
        case '^' => '>'
        case 'v' => '<'
        case '>' => 'v'
        case '<' => '^'
      }
      Cart(x, y, id, newFacing, if (updateLast) 'R' else lastTurn)
    }

  def stayStraight(updateLast: Boolean = false): UnitState =
    mapCurrent{cart => if (updateLast) cart.copy(lastTurn = 'S') else cart}

  def turnAtSlash: UnitState =
    State.get flatMap {_._2.facing match {
        case '^' => turnRight()
        case 'v' => turnRight()
        case '>' => turnLeft()
        case '<' => turnLeft()
      }
    }

  def turnAtBackslash: UnitState =
    State.get flatMap {_._2.facing match {
        case '^' => turnLeft()
        case 'v' => turnLeft()
        case '>' => turnRight()
        case '<' => turnRight()
      }
    }

  def turnAtPlus: UnitState =
    State.get flatMap {_._2.lastTurn match {
        case 'L' => stayStraight(true)
        case 'S' => turnRight(true)
        case 'R' => turnLeft(true)
      }
    }

  def turn(path: Path): UnitState =
    State.get flatMap {s => 
      val pathChar = path((s._2.x, s._2.y))
      pathChar match {
        case '\\' => turnAtBackslash
        case '/'  => turnAtSlash
        case '+'  => turnAtPlus
      }
    }

  def getCollisions: State[CartState, Collisions] =
    State{case s@(carts, Cart(x, y, _, _, _)) =>
      (s, carts.filter{c => c.x == x && c.y == y}.toSet)
    }

  def getCrashLocation(a: Collisions): Option[(Int, Int)] = a.map{c => (c.x, c.y)}.headOption

  def moveCart(cart: Cart, path: Path, part: Int): State[CartState, Option[(Int, Int)]] = {
    val part1 = for {
      _            <- moveForward
      _            <- turn(path)
      collisions   <- getCollisions
      crashLocation = getCrashLocation(collisions)
    } yield crashLocation

    val part2 = part1
    // remove collided carts
    // check for single cart
    // output single cart location

    if (part == 1) part1 else part2
  }

  def sortCartList[F[_]](implicit f: Applicative[F]): IndexedStateT[F, CartState, List[Cart], Unit] =
    IndexedStateT{case s@(carts, cart) =>
       f.pure((carts.toList.sortBy{c => (c.x, c.y)}, ()))
    }

  // def moveAllCartsOnce(path: Path, part: Int): State[CartState, Option[(Int, Int)]] = {
  //   for {
  //     cart  <- sortCartList
  //     result <- moveCart(cart, path, part)
  //   } yield result
  // }

  override def answer1: String = firstCrash(source).toString
  override def answer2: String = lastManStanding(source).toString
  // 24, 57 is wrong
  // 148, 109 is wrong
  // 149, 109 is wrong
  // 147, 109 is wrong
}
