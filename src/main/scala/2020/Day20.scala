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
    val tiles = input.map(parseTile)
    val unsharedBorders = tiles.flatMap(tile => tile.borders.map(border => (border -> tile))).groupBy(_._1).filterNot(_._2.size >= 2).map(_._1).toSet
    val corners = tiles.filter(_.borders.count(unsharedBorders contains _) == 4)
    corners.map(_.borders.zipWithIndex.filter(unsharedBorders contains _._1)).foreach(println)
    val topLeft = corners.find(corner => unsharedBorders.contains(corner.borders(0)) && unsharedBorders.contains(corner.borders(2))).get
    println(topLeft)
    ???

  case class Tile(number: Long, points: Set[Matrix[3, 1, Int]]):
    lazy val borders: Vector[Int] =
      val top = points.filter(_.element[1, 0] == 0).map(_.element[0, 0]).map(x => Math.pow(2, x).toInt).sum
      val bottom = points.filter(_.element[1, 0] == 9).map(_.element[0, 0]).map(x => Math.pow(2, x).toInt).sum
      val left = points.filter(_.element[0, 0] == 0).map(_.element[1, 0]).map(y => Math.pow(2, y).toInt).sum
      val right = points.filter(_.element[0, 0] == 9).map(_.element[1, 0]).map(y => Math.pow(2, y).toInt).sum
      val rtop = points.filter(_.element[1, 0] == 0).map(_.element[0, 0]).map(x => Math.pow(2, (9 - x)).toInt).sum
      val rbottom = points.filter(_.element[1, 0] == 9).map(_.element[0, 0]).map(x => Math.pow(2, (9 - x)).toInt).sum
      val rleft = points.filter(_.element[0, 0] == 0).map(_.element[1, 0]).map(y => Math.pow(2, (9 - y)).toInt).sum
      val rright = points.filter(_.element[0, 0] == 9).map(_.element[1, 0]).map(y => Math.pow(2, (9 - y)).toInt).sum
      Vector(top, bottom, left, right, rtop, rbottom, rleft, rright)

  def parseTile(input: List[String]): Tile =
    val regex = "Tile (\\d+):".r
    val number = input.head match
      case regex(num) => num.toInt
    val grid = summon[Read[Grid[Char]]].read(input.tail.mkString("\n"))
    val points = (0 to 9).toSet.flatMap{row =>
      (0 to 9).toSet.flatMap{col => if grid(Pos(row, col)) == '#' then Some(Matrix[1, 3, Int](Vector(Vector(col, row, 1))).getOrElse(throw Exception("bad matrix")).transpose) else None}
    }
    Tile(number, points)
