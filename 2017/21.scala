import scala.io.Source

type Square = (Int, Int)
type Grid = Set[Square]

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

def flipX(offset: Int)(in: Grid): Grid = {
  in map{case (x, y) => (-1 * x + offset, y)}
}

def flipY(offset: Int)(in: Grid): Grid = {
  in map{case (x, y) => (x, -1 * y + offset)}
}

def allTransforms(offset: Int)(in: Grid): Set[Grid] = {
  val ro = rotate(offset) _
  val fx = flipX(offset) _
  val fy = flipY(offset) _

  val transforms: Set[(Grid) => Grid] = Set(
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

def parseMap(offset: Int, in: List[String]): Map[Grid, Grid] = {
  //in.map(parseLine).flatMap{x => allTransforms(offset)(x._1).map((_, x._2))}.toMap
  in.map(parseLine).toMap
}

val input = Source.fromFile("input21.txt").getLines.toList
val (twoByTwoRaw, threeByThreeRaw) = input.partition(_(2) == '/')
val twoByTwo = parseMap(1, twoByTwoRaw)
val threeByThree = parseMap(2, threeByThreeRaw)

def splitGrid(size: Int, grid: Grid): Seq[Grid] = {
  val splitSize = if (size % 2 == 0) 2 else 3
  for {
    x <- 0 until (size / splitSize)
    y <- 0 until (size / splitSize)
  } yield {
    grid.filter{case (gridX, gridY) => gridX / splitSize == x && gridY / splitSize == y}
      .map{case (gridX, gridY) => (gridX % splitSize, gridY % splitSize)}
  }
}

def joinGrid(size: Int, grids: Seq[Grid]): Grid = {
  val splitSize = if (size % 2 == 0) 2 else 3
  val remapped: Seq[Grid] = grids.zipWithIndex.map{case (grid, index) => (grid, (index % splitSize, index / splitSize))}
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
  val enhancer = if (size % 2 == 0) twoByTwo else threeByThree
  val enhanced = split.map(enhancer)
  val newSize = if (size % 2 == 0) size + (size / 2) else size + (size / 3)
  val newGrid = joinGrid(size, enhanced)
  (newSize, newGrid)
}

println("Non transformed:")
val nonTransformed = parseGrid("#../###/...")
allTransforms(2)(nonTransformed) map {x => gridToString(3, x)} foreach println
println("Not found:")
println(gridToString(3, Set((0,0), (1,1), (1,2), (1,0))))
//val answer1 = iterations.drop(5).next._2.size
