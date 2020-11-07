package advent2018
import common.Day
import scala.io.Source

class Day3(source: Source) extends Day {
  val input = source.getLines().toList

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int)

  def parseClaim(in: String): Claim = {
    val claim = """#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)""".r
    in match {
      case claim(id, left, top, width, height) => Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
    }
  }

  val claims = input map parseClaim

  type ClaimMap = Map[(Int, Int), Set[Int]]

  def getClaimMap(claim: Claim): ClaimMap = {
    val squares = for {
      left <- (claim.left until (claim.left + claim.width))
      top  <- (claim.top  until (claim.top  + claim.height))
    } yield ((left, top) -> Set(claim.id))
    squares.toMap
  }

  // TODO: Make generic function in Dynamic? Use cats-collection diet?
  def mergeMaps(first: ClaimMap, second: ClaimMap): ClaimMap = {
    second.foldLeft(first){case (accum, (nextIndex, nextId)) =>
      val existing: Set[Int] = accum.getOrElse(nextIndex, Set.empty[Int])
      accum + ((nextIndex, (nextId ++ existing)))
    }
  }

  val claimMaps = claims map getClaimMap

  val mergedMaps = claimMaps reduce mergeMaps

  val overlappingClaims = mergedMaps filter {_._2.size > 1}

  override def answer1 = (overlappingClaims.size).toString

  val overlappingIds = overlappingClaims.flatMap{_._2}.toSet
  val allIds = mergedMaps.flatMap{_._2}.toSet

  override def answer2 = (allIds -- overlappingIds).head.toString
}
