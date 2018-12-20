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

  sealed trait Regex
  case class Move(direction: Char)           extends Regex
  case class Branch(children: List[Regex])   extends Regex
  case class Sequence(children: List[Regex]) extends Regex

  final def parseRegex(regex: String): Regex = {
    parseSequence(regex drop 1)._2
  }

  final def parseMove(regex: String): (String, Move) = {
    (regex.tail, Move(regex.head))
  }

  final def parseBranch(regex: String, accum: List[Regex] = List.empty[Regex]): (String, Branch) = {
    if (regex.isEmpty || (")$" contains regex.head)) {
      (regex, Branch(accum.reverse))
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

  override def answer1: String = ???
  override def answer2: String = ???
}
