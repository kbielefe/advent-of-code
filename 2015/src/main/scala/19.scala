package day19
import parse.{*, given}
import scala.util.Random
import scala.annotation.tailrec

case class Replacement(lhs: String, rhs: String):
  def singleReplacement(molecule: String): Set[String] =
    lhs.r.findAllMatchIn(molecule).map(m => molecule.patch(m.start, rhs, lhs.size)).toSet

  def reverseReplacement(molecule: String): Set[String] =
    rhs.r.findAllMatchIn(molecule).map(m => molecule.patch(m.start, lhs, rhs.size)).toSet

given Read[Replacement] = Read("""(.+) => (.+)""".r)

case class Input(replacements: List[Replacement], molecule: String):
  def singleReplacements: List[String] =
    replacements.flatMap(_.singleReplacement(molecule))

  def synthesizeMolecule: Int =
    @tailrec
    def helper(count: Int, accum: String): Int =
      if accum == "e" then
        count
      else
        Random.shuffle(replacements).flatMap(_.reverseReplacement(accum)).headOption match
          case Some(newAccum) => helper(count + 1, newAccum)
          case None           => helper(0, molecule)
    helper(0, molecule)

given Read[List[Replacement]] = Read("\n")
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Int, Int]:
  def part1(input: Input): Int =
    input.singleReplacements.size

  def part2(input: Input): Int =
    input.synthesizeMolecule
