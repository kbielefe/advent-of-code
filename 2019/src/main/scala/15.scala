package day15

import algorithms.AStar
import cats.effect.IO
import cats.implicits.*
import cats.syntax.all.*
import parse.{*, given}
import scala.annotation.tailrec
import year2019.IntCode

type I = Vector[Long]
given Read[I] = Read(",")
type P = (Int, Int)

def neighbors(p: P): Set[P] =
  val (x, y) = p
  Set((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))

def manhattan(from: P)(to: P): Int =
  val (fromX, fromY) = from
  val (toX, toY) = to
  Math.abs(fromX - toX) + Math.abs(fromY - toY)

class Droid(computer: IntCode):
  def mapShip: IO[(P, Set[P])] =
    explore(Set((0, 0)), neighbors((0, 0)), Set((0, 0)), None, (0, 0))

  def move(from: P, to: P): Long =
    val xDiff = to._1 - from._1
    val yDiff = to._2 - from._2
    (xDiff, yDiff) match
      case ( 1,  0) => 4
      case (-1,  0) => 3
      case ( 0,  1) => 1
      case ( 0, -1) => 2

  private def explore(explored: Set[P], toExplore: Set[P], spaces: Set[P], oxygen: Option[P], location: P): IO[(P, Set[P])] =
    if toExplore.isEmpty then
      IO.pure((oxygen.get, spaces))
    else
      val unexploredNeighbors = neighbors(location) & toExplore
      if unexploredNeighbors.isEmpty then
        val backtrackTo = toExplore.minBy(manhattan(location))
        val neighbor = (neighbors(backtrackTo) & spaces).head
        val astar = AStar[P, Int](_ == neighbor, manhattan(neighbor), (_, _) => 1, 0, p => neighbors(p) & spaces)
        val path = astar.getPath(location)
        val moves = path.sliding(2).map{case Seq(from, to) => move(from, to)}
        moves.toList.traverse{move =>
          computer.input(move) >> computer.output.map(result => assert(result != 0))
        } >>
        explore(explored, toExplore, spaces, oxygen, neighbor)
      else
        val exploring = unexploredNeighbors.head
        val newExplored = explored + exploring
        val command = move(location, exploring)
        computer.input(command) >> computer.output.flatMap{response =>
          val newLocation = response match
            case 0 => location
            case _ => exploring
          val newSpaces = response match
            case 0 => spaces
            case _ => spaces + exploring
          val newOxygen = response match
            case 2 => Some(exploring)
            case _ => oxygen
          val newToExplore = response match
            case 0 => toExplore - exploring
            case _ => toExplore ++ neighbors(exploring) -- newExplored
          explore(newExplored, newToExplore, newSpaces, newOxygen, newLocation)
        }

object Puzzle extends runner.IODay[I, Int, Int]:
  def part1(input: I): IO[Int] = for
    computer         <- IntCode(input)
    droid             = Droid(computer)
    fiber            <- computer.run.start
    (oxygen, spaces) <- droid.mapShip
    astar = AStar[P, Int](_ == oxygen, manhattan(oxygen), (_, _) => 1, 0, p => neighbors(p) & spaces)
    result = astar.getMinCost((0, 0)).get
    _ <- fiber.cancel
  yield result

  def part2(input: I): IO[Int] = for
    computer         <- IntCode(input)
    droid             = Droid(computer)
    fiber            <- computer.run.start
    (oxygen, spaces) <- droid.mapShip
    _                <- fiber.cancel
  yield maxDepth(spaces, Set.empty, Set(oxygen -> 0), 0)

  @tailrec
  def maxDepth(spaces: Set[P], visited: Set[P], toVisit: Set[(P, Int)], accum: Int): Int =
    if toVisit.isEmpty then
      accum
    else
      val visiting = toVisit.head
      val (location, depth) = visiting
      val newNeighbors = (neighbors(location) & spaces) -- visited -- toVisit.map(_._1)
      val newAccum = Math.max(accum, depth)
      val newVisited = visited + location
      val newToVisit = newNeighbors.map(_ -> (depth + 1)) ++ toVisit - visiting
      maxDepth(spaces, newVisited, newToVisit, newAccum)
