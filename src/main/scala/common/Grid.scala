package common
import scala.io.Source
import monix.tail.Iterant
import cats.Applicative
import cats.effect.Sync

// Internal coordinates have (x, y) where (0, 0) is at top left
class Grid[Cell <: Grid.Cell](private val zorders: Map[(Int, Int), List[Cell]]) {
  val top    = zorders.keySet.map{_._2}.min
  val bottom = zorders.keySet.map{_._2}.max
  val left   = zorders.keySet.map{_._1}.min
  val right  = zorders.keySet.map{_._1}.max

  def move(from: (Int, Int), to: (Int, Int)): Grid[Cell] = {
    val (x, y) = from
    getCell(x, y) map {c => delete(from).add(to, c)} getOrElse this
  }

  def replace(coords: (Int, Int), cell: Cell): Grid[Cell] = {
    delete(coords).add(coords, cell)
  }

  def add(coords: (Int, Int), cell: Cell): Grid[Cell] = {
    val z = zorders.getOrElse(coords, List.empty[Cell])
    val newZorders = zorders + ((coords, cell :: z))
    new Grid(newZorders)
  }

  def delete(coords: (Int, Int)): Grid[Cell] = {
    val z = zorders.getOrElse(coords, List.empty[Cell])
    if (z.isEmpty) {
      this
    } else {
      val newZorders = zorders + ((coords, z.tail))
      new Grid(newZorders)
    }
  }

  def readingOrder(p: (Cell) => Boolean = _ => true): List[(Int, Int)] = {
    val all = for {
      y <- (top to bottom).toList
      x <- (left to right).toList
    } yield (x, y)
    all
      .filter{case (x, y) => zorders contains (x, y)}
      .filter{case (x, y) => p(getCell(x, y).get)}
  }

  /**
   * Runs turn for every cell in the cellOrder, which order is recalculated at the end of every round.
   */
  def rounds[F[_], A](turn: (Grid[Cell], (Int, Int)) => (Grid[Cell], A),
                     cellOrder: (Grid[Cell]) => List[(Int, Int)] = _.readingOrder())(
                     implicit F: Sync[F]
                     ): Iterant[F, (Grid[Cell], A)] = {
    def recurse(grid: Grid[Cell], order: List[(Int, Int)]): Iterant[F, (Grid[Cell], A)] = {
      if (order.isEmpty) {
        recurse(grid, cellOrder(grid))
      } else {
        val (coords :: remainingOrder) = order
        val (newGrid, result) = turn(grid, coords)
        Iterant.pure((newGrid, result)) ++ recurse(newGrid, remainingOrder)
      }
    }
    recurse(this, List.empty)
  }

  def getCell(x: Int, y: Int): Option[Cell] = zorders.get(x, y) map {_.head}

  def getStack(x: Int, y: Int): List[Cell] = zorders.getOrElse((x, y), List.empty[Cell])

  def getLines(empty: Char = ' '): Iterator[String] = {
    def getChar(x: Int, y: Int) = getCell(x, y) map {_.char} getOrElse empty
    def getLine(y: Int) = (left to right).map(getChar(_, y)).mkString
    (top to bottom).iterator map getLine
  }
}

object Grid {
  def apply[C <: Cell](
    top:    Int,
    left:   Int,
    empty:  Char,
    source: Source,
    under:  Char => Option[Char],
    toCell: Char => C): Grid[C] = {
      val (_, zorders) = source.getLines.foldLeft((top, Map.empty[(Int, Int), List[C]])){case ((y, zorders), line) =>
        (y + 1, zorders ++ parseRow(y, left, empty, line, under, toCell))
      }
      new Grid(zorders)
    }

  def apply[C <: Cell](
    top:    Int,
    left:   Int,
    empty:  Char,
    string: String,
    under:  Char => Option[Char],
    toCell: Char => C): Grid[C] = apply(top, left, empty, Source.fromString(string), under, toCell)

  private def parseRow[C <: Cell](
    y:      Int,
    left:   Int,
    empty:  Char,
    string: String,
    under:  Char => Option[Char],
    toCell: Char => C): Map[(Int, Int), List[C]] = {
      val indexedString = string.zipWithIndex.map{case (s, i) => (s, i + left)}

      indexedString.foldLeft(Map.empty[(Int, Int), List[C]]){case (zorders, (char, x)) =>
        if (char == empty) {
          zorders
        } else {
          val zorder = Iterator.iterate(Some(char): Option[Char]){_ flatMap under}
            .takeWhile{_.isDefined}
            .map{_.get}
            .flatMap(x => if (x == empty) None else Some(toCell(x)))
            .toList

            zorders + (((x, y), zorder))
        }
      }
    }

  trait Cell {
    def char: Char
  }
}
