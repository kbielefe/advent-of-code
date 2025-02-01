package advent2018
import common.{Day, ElfCode}
import common.ElfCode.Instruction
import scala.io.Source
import scala.annotation.tailrec
import scala.util.matching.Regex.Match

class Day16(source: Source) extends Day {

  val instructionRegex = """(\d+) (\d+) (\d+) (\d+)""".r
  val beforeRegex = """Before:\s+\[(\d+), (\d+), (\d+), (\d+)\]""".r
  val afterRegex  = """After:\s+\[(\d+), (\d+), (\d+), (\d+)\]""".r

  case class Test(before: Vector[Int], instruction: Vector[Int], after: Vector[Int])

  lazy val lines = source.getLines()

  //TODO: Make some sort of generic multiline regex parser or state machine parser
  @tailrec
  final def parseInput(tests: List[Test],
                 instructions: List[Vector[Int]],
                 before: Vector[Int],
                 instruction: Vector[Int]): (List[Test], List[Vector[Int]]) = {
    def matchToVector(m: Option[Match]): Vector[Int] = m.map{_.subgroups.map{_.toInt}.toVector}.getOrElse(Vector.empty[Int])

    if (!lines.hasNext) {
      (tests.reverse, instructions.reverse)
    } else if (instructions.isEmpty) {
      val line = lines.next
      val instructionMatch = instructionRegex.findFirstMatchIn(line)
      val beforeMatch = beforeRegex.findFirstMatchIn(line)
      val afterMatch = afterRegex.findFirstMatchIn(line)
      if (afterMatch.isDefined) {
        val newTest = Test(before, instruction, matchToVector(afterMatch))
        parseInput(newTest :: tests, List.empty, Vector.empty, Vector.empty)
      } else if (beforeMatch.isDefined) {
        parseInput(tests, List.empty, matchToVector(beforeMatch), Vector.empty)
      } else if (instructionMatch.isDefined && before.isEmpty) {
        parseInput(tests, matchToVector(instructionMatch) :: Nil, Vector.empty, Vector.empty)
      } else if (instructionMatch.isDefined) {
        parseInput(tests, List.empty, before, matchToVector(instructionMatch))
      } else { // blank line
        parseInput(tests, List.empty, Vector.empty, Vector.empty)
      }
    } else {
      val newInstruction = matchToVector(instructionRegex.findFirstMatchIn(lines.next))
      parseInput(tests, newInstruction :: instructions, Vector.empty, Vector.empty)
    }
  }

  lazy val (tests, program) = parseInput(List.empty, List.empty, Vector.empty, Vector.empty)

  def matchesInstruction(test: Test)(instruction: Instruction): Boolean = {
    instruction(test.instruction, test.before) == test.after
  }

  def matchesThreeInstructions(test: Test): Boolean = {
    ElfCode.instructions.count(matchesInstruction(test)) >= 3
  }

  override def answer1: String = tests.count(matchesThreeInstructions).toString
  override def answer2: String = ElfCode.execute(program)(0).toString
}
