package advent2021
import puzzleparse.{MultiLine, Read, given}
import annotation.tailrec

object Day19:
  def part1(input: MultiLine[List[String]]): Int =
    val scanners = input.map(parseScanner)
    val normalized = normalizeAll(scanners, Set.empty)
    normalized.flatMap(_.beacons).size

  def part2(input: MultiLine[List[String]]): Int =
    val scanners = input.map(parseScanner)
    val normalized = normalizeAll(scanners, Set.empty)
    normalized.toList.combinations(2).map{case List(x, y) => x.distance(y)}.max

  @tailrec
  def normalizeAll(toNormalize: List[Scanner], normalized: Set[Scanner]): Set[Scanner] =
    toNormalize match
      case Nil => normalized
      case head :: tail =>
        val overlaps = tail.flatMap(_.overlaps(head))
        val overlapsNumbers = overlaps.toSet.map(_.number)
        val notOverlaps = tail.filterNot(scanner => overlapsNumbers.contains(scanner.number))
        normalizeAll(overlaps ++ notOverlaps, normalized + head)

  case class Scanner(number: Int, position: (Int, Int, Int), beacons: List[Beacon]) derives CanEqual:
    override def toString: String =
      s"--- scanner $number ---\n" + beacons.sortBy(b => (b.x, b.y, b.z)).mkString("\n") + "\n"

    def overlaps(other: Scanner): Option[Scanner] =
      rotations.flatMap{rotation =>
        val translations = for
          ours   <- beacons
          theirs <- other.beacons
        yield rotation * ours - theirs
        translations.groupBy(identity).find(_._2.size >= 12).map(t => (rotation, t._1.convert))
      }.headOption.map(normalize.tupled)

    def normalize(rotation: Matrix, translation: Beacon): Scanner =
      val transform = rotation + translation.translation
      val newPosition = (translation.x, translation.y, translation.z)
      val newBeacons = beacons.map(transform * _).map(_.convert)
      Scanner(number, newPosition, newBeacons)

    def distance(other: Scanner): Int =
      (position._1 - other.position._1).abs + (position._2 - other.position._2).abs + (position._3 - other.position._3).abs

  case class Beacon(x: Int, y: Int, z: Int):
    override def toString: String =
      s"$x,$y,$z"

    def translation: Matrix =
      Matrix(List(List(0, 0, 0, -x),
                   List(0, 0, 0, -y),
                   List(0, 0, 0, -z),
                   List(0, 0, 0, 0)))

  given Conversion[Beacon, Matrix] with
    def apply(beacon: Beacon): Matrix =
      Matrix(List(List(beacon.x, beacon.y, beacon.z, 1)).transpose)

  given Conversion[Matrix, Beacon] with
    def apply(matrix: Matrix): Beacon =
      val List(x, y, z, _) = matrix.rows.transpose.flatten
      Beacon(x, y, z)

  case class Matrix(rows: List[List[Int]]):
    override def toString: String =
      val width = rows.flatten.map(_.toString.size).max
      "\n" + rows.map(_.map(_.toString).map(padTo(width)).mkString(" ")).mkString("\n")

    private def padTo(width: Int)(string: String): String =
      " " * (width - string.size) + string

    def -(other: Matrix): Matrix =
      Matrix(rows.zip(other.rows).map(_.zip(_).map(_ - _)))

    def +(other: Matrix): Matrix =
      Matrix(rows.zip(other.rows).map(_.zip(_).map(_ + _)))

    def scalarMult(mult: Int): Matrix =
      Matrix(rows.map(_.map(_ * mult)))

    def *(other: Matrix): Matrix =
      val cols = other.rows.transpose
      val resultRows = rows.map(row =>
        cols.map(col =>
          row.zip(col).map(_ * _).sum
        )
      )
      Matrix(resultRows)

  def parseScanner(input: List[String]): Scanner =
    val number = input.head.drop(12).takeWhile(_.isDigit).toInt
    val beacons = input.tail.map(summon[Read[Beacon]].read)
    Scanner(number, (0, 0, 0), beacons)

  def rotation(input: String): Matrix =
    val rows = input
      .split("\n")
      .toList
      .map(_.trim.split("\\s+").toList.map(_.trim.toInt) :+ 0)
    Matrix(rows :+ List(0, 0, 0, 1))

  val rx = rotation("""1  0  0
                       0  0 -1
                       0  1  0""")

  val ry = rotation("""0  0  1
                       0  1  0
                      -1  0  0""")

  val rz = rotation("""0 -1  0
                       1  0  0
                       0  0  1""")

  val i = Matrix(List(List(1, 0, 0, 0), List(0, 1, 0, 0), List(0, 0, 1, 0), List(0, 0, 0, 1)))

  val rotations = for
    facing <- List(i, rx, rx * rx, rx * rx * rx, ry, ry * ry * ry)
    up     <- List(i, rz, rz * rz, rz * rz * rz)
  yield facing * up
