package advent2018
import common.{Day, Grid}
import scala.io.Source

class Day17(source: Source) extends Day {

  type Square = (Int, Int)

  val xRangeRegex = """y=(\d+), x=(\d+)..(\d+)""".r
  val yRangeRegex = """x=(\d+), y=(\d+)..(\d+)""".r

  lazy val clay: Set[Square] = {
    def parseLine(line: String): Seq[Square] = line match {
      case xRangeRegex(y, xMin, xMax) => (xMin.toInt to xMax.toInt) map {x => (x, y.toInt)}
      case yRangeRegex(x, yMin, yMax) => (yMin.toInt to yMax.toInt) map {y => (x.toInt, y)}
    }
     source.getLines.flatMap(parseLine).toSet
  }
  
  lazy val minY = clay.map(_._2).min
  lazy val maxY = clay.map(_._2).max
  lazy val minX = clay.map(_._1).min
  lazy val maxX = clay.map(_._1).max

  @scala.annotation.tailrec
  final def water(fallingSources: Set[Square], expandingSources: Set[(Square, Square)], falling: Set[Square], standing: Set[Square]): (Set[Square], Set[Square]) = {
    def isStillFalling(square: Square) = {
      val (x, y) = square
      val squareBelow = (x, y + 1)
      !(clay.contains(squareBelow) || standing.contains(squareBelow))
    }

    def isBlockedLeft(expanding: (Square, Square)): Boolean = {
      val (x, y) = expanding._1
      clay.contains((x - 1), y)
    }

    def isBlockedRight(expanding: (Square, Square)): Boolean = {
      val (x, y) = expanding._2
      clay.contains((x + 1), y)
    }

    def isFallingLeft(expanding: (Square, Square)): Boolean = isStillFalling(expanding._1)
    def isFallingRight(expanding: (Square, Square)): Boolean = isStillFalling(expanding._2)

    def expand(expanding: (Square, Square)): (Square, Square) = {
      val ((leftX, leftY), (rightX, rightY)) = expanding
      val newLeft = if (clay.contains((leftX - 1, leftY)) || isFallingLeft(expanding)) (leftX, leftY) else (leftX - 1, leftY)
      val newRight = if (clay.contains((rightX + 1, rightY)) || isFallingRight(expanding)) (rightX, rightY) else (rightX + 1, rightY)
      (newLeft, newRight)
    }

    def fall(square: Square): Square = {
      val (x, y) = square
      (x, y + 1)
    }

    def above(square: Square): Square = {
      val (x, y) = square
      (x, y - 1)
    }

    if (fallingSources.isEmpty && expandingSources.isEmpty) {
      (falling.filter{case (_, y) => y >= minY && y <= maxY}, standing)
    } else {
      val didntFallOff = fallingSources.filter(_._2 < maxY)
      val (stillFalling, nowExpanding) = didntFallOff.partition(isStillFalling)

      val blockedLeft  = expandingSources.filter(isBlockedLeft)
      val blockedRight = expandingSources.filter(isBlockedRight)
      val fallingLeft  = expandingSources.filter(isFallingLeft)
      val fallingRight = expandingSources.filter(isFallingRight)

      val blockedBoth = blockedLeft & blockedRight
      val fallingBoth = fallingLeft & fallingRight
      val blockedLeftFallingRight = blockedLeft & fallingRight
      val blockedRightFallingLeft = blockedRight & fallingLeft
      val stillExpanding = expandingSources -- blockedBoth -- fallingBoth -- blockedRightFallingLeft -- blockedLeftFallingRight

      val newStanding = blockedBoth.flatMap{case (left, right) => (left._1 to right._1).map(x => (x, left._2))}
      val newFallingSources = stillFalling.map(fall) ++ fallingBoth.flatMap(x => Set(x._1, x._2)) ++ blockedLeftFallingRight.map(_._2) ++ blockedRightFallingLeft.map(_._1)
      val newFalling = stillFalling.map(fall) ++ (fallingBoth ++ blockedLeftFallingRight ++ blockedRightFallingLeft).flatMap{case ((leftX, y), (rightX, _)) => (leftX to rightX).map(x => (x, y)).toSet}
      val newExpandingSources = stillExpanding.map(expand) ++ nowExpanding.map(x => (x, x)) ++ newStanding.map(above).filter(falling contains _).map(x => (x, x))
      water(newFallingSources, newExpandingSources, falling ++ newFalling, standing ++ newStanding)
    }
  }

  override def answer1: String = {
    val (falling, standing) = water(Set((500, 0)), Set.empty, Set.empty, Set.empty)
    (falling ++ standing).size.toString
  }

  override def answer2: String = {
    val (_, standing) = water(Set((500, 0)), Set.empty, Set.empty, Set.empty)
    standing.size.toString
  }
}
