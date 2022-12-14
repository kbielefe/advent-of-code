package advent2015
import scala.concurrent.{Future, Promise}
import scala.util.Try

object Day7:
  def part1(input: List[String]): Future[Int] =
    // TODO: Change to DFS from "a"
    input.foldLeft(Map.empty[String, Promise[Int]]){
      case (wires, s"$x AND $y -> $z") => ???
      case (wires, s"$x OR $y -> $z") => ???
      case (wires, s"$x LSHIFT $y -> $z") => ???
      case (wires, s"$x RSHIFT $y -> $z") => ???
      case (wires, s"NOT $y -> $z") => ???
      case (wires, s"$y -> $z") => ???
    }("a").future

  def part2(input: List[String]): Int =
    ???

  private def dependencies(input: List[String]): Map[String, List[String]] =
    ???
