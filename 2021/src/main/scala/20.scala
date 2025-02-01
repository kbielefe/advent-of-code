package advent2021
import puzzleparse.{*, given}

object Day20:
  def part1(input: String): Int =
    enhanceNTimes(input, 2)

  def part2(input: String): Int =
    enhanceNTimes(input, 50)

  def enhanceNTimes(input: String, n: Int): Int =
    val Array(algorithmString, imageString) = input.split("\n\n")
    val algorithm = algorithmString.toVector.map(_ == '#')
    val image = InfiniteImage(algorithm, summon[Read[Grid[Char]]].read(imageString).filter(_._2 == '#').keySet, true)
    val enhanced = Iterator.iterate(image)(_.enhance).drop(n).next
    println(enhanced)
    enhanced.known.size

  case class InfiniteImage(algorithm: Vector[Boolean], known: Set[Pos], lit: Boolean):
    override def toString: String =
      val minRow = known.map(_.row).min
      val minCol = known.map(_.col).min
      val maxRow = known.map(_.row).max
      val maxCol = known.map(_.col).max
      " " + (minCol to maxCol).map(_.abs % 10).mkString + "\n" +
      (minRow to maxRow).map{row =>
        (row.abs % 10).toString + (minCol to maxCol).map{col =>
          if known.contains(Pos(row, col)) == lit then '█' else ' '
        }.mkString
      }.mkString("\n")

    def enhance: InfiniteImage =
      val minRow = known.map(_.row).min - 2
      val minCol = known.map(_.col).min - 2
      val maxRow = known.map(_.row).max + 2
      val maxCol = known.map(_.col).max + 2
      val pixels = for
        row <- (minRow to maxRow).toSet
        col <- (minCol to maxCol).toSet
      yield Pos(row, col)
      val newLit = if lit then !algorithm(0) else !algorithm(0x1ff)
      val newKnown = pixels.flatMap(replacePixel(newLit))
      InfiniteImage(algorithm, newKnown, newLit)

    def replacePixel(newLit: Boolean)(pixel: Pos): Option[Pos] =
      val digits = for
        row <- ((pixel.row - 1) to (pixel.row + 1)).iterator
        col <- ((pixel.col - 1) to (pixel.col + 1)).iterator
      yield if known.contains(Pos(row, col)) == lit then 1 else 0
      val index = digits.foldLeft(0)((acc, digit) => (acc << 1) + digit)
      Some(pixel).filter(_ => algorithm(index) == newLit)
