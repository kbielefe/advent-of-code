package advent2019
import common.Day
import common.Numeric._
import scala.io.Source

class Day5(source: Source) extends Day {
  val initialProgram = source.getLines().next().split(",").map(_.toInt).toVector

  def getParameter(program: Vector[Int], pc: Int, param: Int): Int = {
    val mode = program(pc).digits.reverse.drop(1).toVector.applyOrElse[Int, Int](param, _ => 0)
    if (mode == 0) // position
      program(program(pc + param))
    else // immediate
      program(pc + param)
  }

  def binaryOp(op: (Int, Int) => Int)(program: Vector[Int], pc: Int): Vector[Int] = {
    val result = op(getParameter(program, pc, 1), getParameter(program, pc, 2))
    program.updated(program(pc + 3), result)
  }

  val add = binaryOp(_ + _) _
  val multiply = binaryOp(_ * _) _

  def jumpIfTrue(program: Vector[Int], pc: Int): Int = {
    if (getParameter(program, pc, 1) != 0)
      getParameter(program, pc, 2)
    else
      pc + 3
  }

  def jumpIfFalse(program: Vector[Int], pc: Int): Int = {
    if (getParameter(program, pc, 1) == 0)
      getParameter(program, pc, 2)
    else
      pc + 3
  }

  def lessThan(program: Vector[Int], pc: Int): Vector[Int] = {
    if (getParameter(program, pc, 1) < getParameter(program, pc, 2))
      program.updated(program(pc + 3), 1)
    else
      program.updated(program(pc + 3), 0)
  }

  def equalTo(program: Vector[Int], pc: Int): Vector[Int] = {
    if (getParameter(program, pc, 1) == getParameter(program, pc, 2))
      program.updated(program(pc + 3), 1)
    else
      program.updated(program(pc + 3), 0)
  }

  def getInput(program: Vector[Int], pc: Int, inputs: List[Int]): Vector[Int] =
    program.updated(program(pc + 1), inputs.head)

  // Outputs are added to list in reverse order, so the head is the last value output
  def addOutput(program: Vector[Int], pc: Int, outputs: List[Int]): List[Int] =
    getParameter(program, pc, 1) :: outputs

  def opcode(instruction: Int): Int = instruction % 100

  @scala.annotation.tailrec
  final def run(program: Vector[Int], pc: Int, inputs: List[Int], outputs: List[Int]): List[Int] = opcode(program(pc)) match {
    case 99 => outputs
    case  1 => run(add(program, pc), pc + 4, inputs, outputs)
    case  2 => run(multiply(program, pc), pc + 4, inputs, outputs)
    case  3 => run(getInput(program, pc, inputs), pc + 2, inputs.tail, outputs)
    case  4 => run(program, pc + 2, inputs, addOutput(program, pc, outputs))
    case  5 => run(program, jumpIfTrue(program, pc), inputs, outputs)
    case  6 => run(program, jumpIfFalse(program, pc), inputs, outputs)
    case  7 => run(lessThan(program, pc), pc + 4, inputs, outputs)
    case  8 => run(equalTo(program, pc), pc + 4, inputs, outputs)
  }

  override def answer1 = run(initialProgram, 0, List(1), List.empty).head.toString

  override def answer2 = run(initialProgram, 0, List(5), List.empty).head.toString
}
