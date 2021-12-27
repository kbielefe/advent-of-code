package advent2020
import puzzleparse.{*, given}
import kbielefe.puzzle.Matrix

object Day20:
  def part1(input: MultiLine[List[String]]): Long =
    val tiles = input.map(parseTile)
    val unsharedBorders = tiles.flatMap(tile => tile.borders.map(border => (border -> tile))).groupBy(_._1).filterNot(_._2.size >= 2).map(_._1).toSet
    val corners = tiles.filter(_.borders.count(unsharedBorders contains _) == 4)
    corners.map(_.number).product

  def part2(input: MultiLine[List[String]]): Long =
    ???

  case class Tile(number: Long, matrix: Matrix[10, 10, Int]):
    def borders: Vector[Int] =
      val powersOf2 = List.iterate(1, 10)(_ * 2)
      val p2Matrix = Matrix[2, 10, Int](Iterator(powersOf2, powersOf2.reverse)).getOrElse(throw Exception("matrix construction error"))
      val end = 1 :: List.fill(9)(0)
      val corners = Matrix[2, 10, Int](List(end, end.reverse)).getOrElse(throw Exception("matrix construction error")).transpose
      (p2Matrix * matrix * corners).elements ++
      (p2Matrix * matrix.transpose * corners).elements

  def parseTile(input: List[String]): Tile =
    val regex = "Tile (\\d+):".r
    val number = input.head match
      case regex(num) => num.toInt
    val grid = summon[Read[Grid[Char]]].read(input.tail.mkString("\n"))
    val rows = (0 to 9).map{row =>
      (0 to 9).map{col => if grid(Pos(row, col)) == '#' then 1 else 0}
    }
    Matrix[10, 10, Int](rows) match
      case Left(_)       => throw Exception("invalid matrix")
      case Right(matrix) => Tile(number, matrix)
