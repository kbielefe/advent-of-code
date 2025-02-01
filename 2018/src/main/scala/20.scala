package advent2018
import common.Day
import scala.io.Source
import monix.tail.Iterant
import monix.eval.Coeval
import cats.data.{State, NonEmptyList}
import cats.syntax.list._
import scala.annotation.tailrec

class Day20(source: Source) extends Day {
  type Point = (Int, Int)

  // TODO: make a generic Tree class that can read and traverse directly from the string
  sealed trait Regex {
    def traverse(endpoints: Set[Point], paths: Map[Point, (Point, Int)]): (Set[Point], Map[Point, (Point, Int)])
  }

  case class Move(direction: Char)           extends Regex {
    override def traverse(endpoints: Set[Point], paths: Map[Point, (Point, Int)]): (Set[Point], Map[Point, (Point, Int)]) = {
      def newPoint(p: Point): Point = (p, direction) match {
        case ((x, y), 'N') => (x, y - 1)
        case ((x, y), 'S') => (x, y + 1)
        case ((x, y), 'E') => (x + 1, y)
        case ((x, y), 'W') => (x - 1, y)
        case ((x, y),   _) => (x , y)
      }
      val newEndpoints = endpoints map newPoint
      val newPaths = endpoints map {p =>
        val length = paths.get(p).map(_._2).getOrElse(0) + 1
        val alreadyExistingPath = paths.get(newPoint(p))
        alreadyExistingPath.map{case (existingParent, existingLength) => if (length < existingLength) (newPoint(p), (p, length)) else (newPoint(p), (existingParent, existingLength))} getOrElse (newPoint(p), (p, length))
      }
      (newEndpoints, paths ++ newPaths)
    }
  }

  case class Branch(children: List[Regex])   extends Regex {
    override def traverse(endpoints: Set[Point], paths: Map[Point, (Point, Int)]): (Set[Point], Map[Point, (Point, Int)]) = {
      val list = children.map{child => child.traverse(endpoints, paths)}
      val newEndpoints = list.flatMap{_._1}.toSet
      val newPaths = list.flatMap{_._2}.toMap
      (newEndpoints, newPaths)
    }
  }

  case class Sequence(children: List[Regex]) extends Regex {
    override def traverse(endpoints: Set[Point], paths: Map[Point, (Point, Int)]): (Set[Point], Map[Point, (Point, Int)]) = {
      children.foldLeft((endpoints, paths)){case ((endpoints, paths), child) => child.traverse(endpoints, paths)}
    }
  }

  final def parseRegex(regex: String): Regex = {
    parseSequence(regex drop 1)._2
  }

  final def parseMove(regex: String): (String, Move) = {
    (regex.tail, Move(regex.head))
  }

  final def parseBranch(regex: String, accum: List[Regex] = List.empty[Regex]): (String, Branch) = {
    if (regex.isEmpty) {
      (regex, Branch(accum.reverse))
    } else if (")$" contains regex.head) {
      (regex.tail, Branch(accum.reverse))
    } else {
      val (nextRegex, nextElement) = regex.head match {
        case 'N' => parseSequence(regex)
        case 'S' => parseSequence(regex)
        case 'E' => parseSequence(regex)
        case 'W' => parseSequence(regex)
        case '(' => parseBranch(regex.tail)
        case '|' => parseSequence(regex.tail)
      }
      parseBranch(nextRegex, nextElement :: accum)
    }
  }

  @tailrec
  final def parseSequence(regex: String, accum: List[Regex] = List.empty[Regex]): (String, Sequence) = {
    if (regex.isEmpty || ("|)$" contains regex.head)) {
      (regex, Sequence(accum.reverse))
    } else {
      val (nextRegex, nextElement) = regex.head match {
        case 'N' => parseMove(regex)
        case 'S' => parseMove(regex)
        case 'E' => parseMove(regex)
        case 'W' => parseMove(regex)
        case '(' => parseBranch(regex.tail)
      }
      parseSequence(nextRegex, nextElement :: accum)
    }
  }

  def furthestRoomDistance(regex: String): Int = {
    val map = parseRegex(regex).traverse(Set((0, 0)), Map.empty)._2
    map.map(_._2._2).max
  }

  def roomsFartherThan(regex: String, distance: Int): Int = {
    val map = parseRegex(regex).traverse(Set((0, 0)), Map.empty)._2
    map.toList.map(_._2._2).count(_ >= distance)
  }

  override def answer1: String = furthestRoomDistance(source.mkString.trim).toString
  override def answer2: String = roomsFartherThan(source.mkString.trim, 1000).toString
}
