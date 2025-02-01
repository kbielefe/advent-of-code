package advent2022
import io.circe.Json
import io.circe.parser.parse
import puzzleparse.MultiLine
import scala.math.Ordering.Implicits.*

object Day13:
  given Ordering[Json] with
    def compare(a: Json, b: Json): Int =
      val intCompare    = summon[Ordering[Int]].compare
      val vectorCompare = summon[Ordering[Vector[Json]]].compare
      (a.isNumber, b.isNumber) match
        case ( true,  true) => intCompare(a.asNumber.flatMap(_.toInt).get, b.asNumber.flatMap(_.toInt).get)
        case (false, false) => vectorCompare(a.asArray.get, b.asArray.get)
        case (false,  true) => vectorCompare(a.asArray.get, Vector(b))
        case ( true, false) => vectorCompare(Vector(a), b.asArray.get)

  def part1(input: MultiLine[List[Json]]): Int =
    input.zipWithIndex.filter{case (List(left, right), _) => left < right}.map(_._2 + 1).sum

  def part2(input: MultiLine[List[Json]]): Int =
    val dividerPackets = List("[[2]]", "[[6]]").map(parse).flatMap(_.toOption)
    val sorted = (dividerPackets ++ input.flatten).sorted
    dividerPackets.map(sorted.indexOf).map(_ + 1).product
