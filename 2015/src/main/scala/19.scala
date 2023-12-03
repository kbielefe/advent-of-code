package day19
import parse.{*, given}
import algorithms.AStar

case class Replacement(lhs: String, rhs: String):
  def singleReplacement(molecule: String): Set[String] =
    lhs.r.findAllMatchIn(molecule).map(m => molecule.patch(m.start, rhs, lhs.size)).toSet

case class Input(replacements: Set[Replacement ~ """(.+) => (.+)"""] - "\n", molecule: String):
  def singleReplacements(molecule: String): Set[String] =
    replacements.flatMap(_.singleReplacement(molecule))

  def medicineSteps: Int =
    val astar = new AStar[String, Int](_ == molecule, heuristic, (_, _) => 1, 0, neighbors)
    astar.getMinCost("e").get

  def neighbors(current: String): Set[String] =
    singleReplacements(current).filter(_.size <= molecule.size)

  lazy val largestReplacement = replacements.map(r => r.rhs.size - r.lhs.size).max

  def heuristic(current: String): Int =
    (Math.abs(current.size - molecule.size) + current.zip(molecule).count(_ != _)) / largestReplacement

type I = Input - "\n\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.singleReplacements(input.molecule).size

  def part2(input: I): Int =
    input.medicineSteps
