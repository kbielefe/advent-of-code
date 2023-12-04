package day19
import parse.{*, given}
import algorithms.Tree
import algorithms.Tree.*

case class Replacement(lhs: String, rhs: String):
  def singleReplacement(molecule: String): Set[String] =
    lhs.r.findAllMatchIn(molecule).map(m => molecule.patch(m.start, rhs, lhs.size)).toSet

  def reverseReplacement(molecule: String): Set[String] =
    rhs.r.findAllMatchIn(molecule).map(m => molecule.patch(m.start, lhs, rhs.size)).toSet

case class Input(replacements: Set[Replacement ~ """(.+) => (.+)"""] - "\n", molecule: String):
  def singleReplacements: Set[String] =
    replacements.flatMap(_.singleReplacement(molecule))

  given Tree[String] = new Tree[String]:
    def children(node: String): Iterator[String] =
      replacements.iterator.flatMap(_.reverseReplacement(node).iterator)

  def medicineSteps: Int =
    molecule.depth(_ == "e").get

type I = Input - "\n\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.singleReplacements.size

  def part2(input: I): Int =
    input.medicineSteps
