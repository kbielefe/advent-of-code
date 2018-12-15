package common
import scala.io.Source

// Internal coordinates have (x, y) where (0, 0) is at top left
class Grid[Cell <: Grid.Cell](private val zorders: Map[(Int, Int), List[Cell]]) {
  val top    = zorders.keySet.map{_._2}.min
  val bottom = zorders.keySet.map{_._2}.max
  val left   = zorders.keySet.map{_._1}.min
  val right  = zorders.keySet.map{_._1}.max

  def move(from: (Int, Int), to: (Int, Int)): Grid[Cell] = {
    val fromZ = zorders.get(from)
    if (fromZ.isEmpty) {
      // Moving from empty onto something still allows that something to show through
      this
    } else {
      val Some(x :: xs) = fromZ
      val toZ = zorders.getOrElse(to, List.empty[Cell])
      val newFromZ = xs
      val newToZ = x :: toZ
      val newZorders = zorders + ((from, newFromZ)) + ((to, newToZ))
      new Grid(newZorders)
    }
  }

  def readingOrder: Iterator[Cell] = {
    val all = for {
      y <- (top to bottom).iterator
      x <- (left to right).iterator
    } yield getCell(x, y)
    all.flatten
  }

  def getCell(x: Int, y: Int): Option[Cell] = zorders.get(x, y) map {_.head}

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
