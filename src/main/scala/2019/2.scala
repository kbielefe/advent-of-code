package advent2019
import common.Day
import scala.io.Source

class Day2(source: Source) extends Day {
  val initialProgram = source.getLines().next().split(",").map(_.toInt).toVector

  def binaryOp(op: (Int, Int) => Int)(program: Vector[Int], pc: Int): Vector[Int] = {
    val result = op(program(program(pc + 1)), program(program(pc + 2)))
    program.updated(program(pc + 3), result)
  }

  val add = binaryOp(_ + _) _
  val multiply = binaryOp(_ * _) _

  @scala.annotation.tailrec
  final def run(program: Vector[Int], pc: Int): Vector[Int] = program(pc) match {
    case 99 => program
    case  1 => run(add(program, pc), pc + 4)
    case  2 => run(multiply(program, pc), pc + 4)
  }

  def runWithInputs(noun: Int, verb: Int): Int =
    run(initialProgram.updated(1, noun).updated(2, verb), 0)(0)

  override def answer1 = runWithInputs(12, 2).toString

  override def answer2 = {
    val results = for {
      noun <- (0 to 99).iterator
      verb <- (0 to 99).iterator
    } yield (noun, verb, runWithInputs(noun, verb))
    val answer = results.dropWhile(_._3 != 19690720).next()
    (answer._1 * 100 + answer._2).toString
  }
}
