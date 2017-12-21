import scala.io.Source

type Square = (Int, Int)
type Grid = Set[Square]

def sizeCompare[A](size: Int, value2: A, value3: A): A =
  if (size % 2 == 0) value2 else value3

def parseGrid(in: String): Grid = {
  in.split("/").zipWithIndex.flatMap{case (row, rowIndex) =>
    row.zipWithIndex.filter(_._1 == '#').map{case (col, colIndex) =>
      (rowIndex, colIndex)
    }
  }.toSet
}

def parseLine(line: String): (Grid, Grid) = {
  val Array(before, after) = line split " => " map parseGrid
  (before, after)
}

def rotate(offset: Int)(in: Grid): Grid = {
  in map{case (x, y) => (-1 * y + offset, x)}
}

def flip(offset: Int)(in: Grid): Grid = {
  in map{case (x, y) => (-1 * x + offset, y)}
}

def allTransforms(offset: Int)(in: Grid): Set[Grid] = {
  val ro = rotate(offset) _
  val f = flip(offset) _

  val transforms: Set[(Grid) => Grid] = Set(
    identity,
    ro,
    ro andThen ro,
    ro andThen ro andThen ro,
    f,
    f andThen ro,
    f andThen ro andThen ro,
    f andThen ro andThen ro andThen ro
  )

  transforms map {_(in)}
}

def parseMap(offset: Int, in: List[String]): Map[Grid, Grid] = {
  in.map(parseLine).flatMap{x => allTransforms(offset)(x._1).map((_, x._2))}.toMap
}

val input = Source.fromFile("input21.txt").getLines.toList
val (twoByTwoRaw, threeByThreeRaw) = input.partition(_(2) == '/')
val twoByTwo = parseMap(1, twoByTwoRaw)
val threeByThree = parseMap(2, threeByThreeRaw)

def splitGrid(size: Int, grid: Grid): Iterator[Grid] = {
  val splitSize = sizeCompare(size, 2, 3)
  for {
    x <- (0 until (size / splitSize)).iterator
    y <- (0 until (size / splitSize)).iterator
  } yield {
    grid.filter{case (gridX, gridY) => gridX / splitSize == x && gridY / splitSize == y}
      .map{case (gridX, gridY) => (gridX % splitSize, gridY % splitSize)}
  }
}

def joinGrid(oldSize: Int, newSize: Int, grids: Iterator[Grid]): Grid = {
  val splitSize = sizeCompare(oldSize, 3, 4)
  val width = newSize / splitSize
  val remapped: Iterator[Grid] = grids.zipWithIndex.map{case (grid, index) => (grid, (index / width, index % width))}
    .map{case (grid, (x, y)) =>
      grid.map{case (gridX, gridY) => (gridX + x * splitSize, gridY + y * splitSize)}
    }
  remapped.foldLeft(Set.empty[Square]){_ ++ _}
}

def gridToString(size: Int, grid: Grid): String = {
  val chars = for {
    x <- 0 until size
    y <- 0 until size
  } yield if (grid contains (x, y)) '#' else '.'
  chars.grouped(size).map{_.mkString}.mkString("", "\n", "\n")
}

val initial = parseGrid(".#./..#/###")
def iterations = Iterator.iterate((3, initial)){case (size, grid) =>
  val split = splitGrid(size, grid)
  val enhancer = sizeCompare(size, twoByTwo, threeByThree)
  val enhanced = split.map(enhancer)
  val newSize = sizeCompare(size, size + (size / 2), size + (size / 3))
  val newGrid = joinGrid(size, newSize, enhanced)
  (newSize, newGrid)
}

def printGrid(grid: Grid, size: Int = 0): Unit = {
  val printSize = if (size > 0)
    size
  else
    grid.flatMap{case (x, y) => List(x, y)}.max + 1
  println(gridToString(printSize, grid))
}

def printGrids(drop: Int): Unit =
  iterations.drop(drop).take(2).map{case (size, grid) => gridToString(size, grid)} foreach println

val answer1 = iterations.drop(5).next._2.size
println(answer1)

//val answer2 = iterations.drop(18).next._2.size
//println(answer2)
