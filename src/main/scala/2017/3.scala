package advent2017
import common.Day
import Math.abs
import scala.io.Source

class Day3(source: Source) extends Day {
  val input = source.mkString.trim.toInt

  val offset = Map('n' -> (0,1), 'e' -> (1,0), 's' -> (0,-1), 'w' -> (-1,0))
  val nextDir = Map('n' -> 'w', 'w' -> 's', 's' -> 'e', 'e' -> 'n')

  def sideLengths = LazyList.from(1).flatMap{x => LazyList(x,x)}

  def turns = sideLengths.flatMap{length => LazyList.fill(length-1)(false) ++ LazyList(true)}

  def dirs = turns.scanLeft('e'){case (dir, turn) => if (turn) nextDir(dir) else dir}

  def coords = dirs.scanLeft((0,0)){case ((x, y), dir) =>
    val (xOff, yOff) = offset(dir)
    (x + xOff, y + yOff)
  }

  def sumOfNeighbors(square: (Int, Int), squares: Map[(Int, Int), Int]): Int = {
    val (x,y) = square
    val neighbors = for {
      neighborX <- x - 1 to x + 1
      neighborY <- y - 1 to y + 1
    } yield squares.getOrElse((neighborX, neighborY), 0)
    neighbors.sum
  }

  def neighbors = coords.scanLeft(Map((0, 0) -> 1)){case (squares, coord) =>
    squares + (coord -> sumOfNeighbors(coord, squares))
  }.drop(1)

  override def answer1 = coords
    .drop(input-1)
    .map{case (x,y) => abs(x)+abs(y)}
    .head.toString

    override def answer2 = (coords zip neighbors)
      .map{case (coord, neighbors) => neighbors(coord)}
      .dropWhile{_ <= input}
      .head.toString
}
