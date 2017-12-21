import scala.io.Source

type Square = (Int, Int)

def parseGrid(in: String): Set[Square] = {
  in.split("/").zipWithIndex.flatMap{case (row, rowIndex) =>
    row.zipWithIndex.filter(_._1 == '#').map{case (col, colIndex) =>
      (rowIndex, colIndex)
    }
  }.toSet
}

def parseLine(line: String): (Set[Square], Set[Square]) = {
  val Array(before, after) = line split " => " map parseGrid
  (before, after)
}

def rotate(offset: Int)(in: Set[Square]): Set[Square] = {
  in map{case (x, y) => (-1 * y + offset, x)}
}

def flipX(offset: Int)(in: Set[Square]): Set[Square] = {
  in map{case (x, y) => (-1 * x + offset, y)}
}

def flipY(offset: Int)(in: Set[Square]): Set[Square] = {
  in map{case (x, y) => (x, -1 * y + offset)}
}

def allTransforms(offset: Int)(in: Set[Square]): Set[Set[Square]] = {
  val ro = rotate(offset) _
  val fx = flipX(offset) _
  val fy = flipY(offset) _

  val transforms: Set[(Set[Square]) => Set[Square]] = Set(
    ro,
    ro andThen ro,
    ro andThen ro andThen ro,
    fx,
    fx andThen ro,
    fx andThen ro andThen ro,
    fx andThen ro andThen ro andThen ro,
    fy,
    fy andThen ro,
    fy andThen ro andThen ro,
    fy andThen ro andThen ro andThen ro
  )

  transforms map {_(in)}
}

def parseMap(offset: Int, in: List[String]): Map[Set[Square], Set[Square]] = {
  in.map(parseLine).flatMap{x => allTransforms(offset)(x._1).map((_, x._2))}.toMap
}

val input = Source.fromFile("input21.txt").getLines.toList
val (twoByTwoRaw, threeByThreeRaw) = input.partition(_(2) == '/')
val twoByTwo = parseMap(1, twoByTwoRaw)
val threeByThree = parseMap(2, threeByThreeRaw)

twoByTwo foreach println
