package common

object Visualize {
  def booleanRowToString(f: (Int) => Boolean)(min: Int, max: Int, charIfTrue: Char, charIfFalse: Char): String = {
    val cells = for {
      x <- min to max
    } yield if (f(x)) charIfTrue else charIfFalse
    cells.mkString
  }

  // returns an iterator of rows
  def gridToString(f: (Int, Int) => Any)(minX: Int, minY: Int, width: Int, height: Int): Iterator[String] = {
    val widths = for {
      x <- (minX to (minX + width  - 1)).iterator
      y <- (minY to (minY + height - 1)).iterator
    } yield f(x, y).toString.size
    val maxWidth = widths.max
    def rightJustify(s: String): String = (" " * (maxWidth - s.size)) + s
    def row(y: Int): String = (minX to (minX + width - 1)).map{x => rightJustify(f(x, y).toString)}.mkString(" ")
    (minY to (minY + height - 1)).iterator.map{y => row(y)}
  }
}
