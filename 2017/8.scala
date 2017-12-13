import scala.io.Source
val input = Source.fromFile("input8.txt")

case class Instruction(reg: String, inc: Boolean, amount: Int, compareReg: String, compareOp: (Int, Int) => Boolean, compareAmount: Int)

object Instruction {
  def apply(in: String): Instruction = {
    val Array(reg, incString, amountString, "if", compareReg, compareOpString, compareAmountString) = in.split(" ")
    val compareOp = compareOpString match {
      case "==" => (x: Int, y: Int) => x == y
      case "!=" => (x: Int, y: Int) => x != y
      case "<=" => (x: Int, y: Int) => x <= y
      case ">=" => (x: Int, y: Int) => x >= y
      case "<"  => (x: Int, y: Int) => x < y
      case ">"  => (x: Int, y: Int) => x > y
    }
    Instruction(reg, incString == "inc", amountString.toInt, compareReg, compareOp, compareAmountString.toInt)
  }
}

val instructions = input.getLines.map(Instruction(_)).toList

def runInstructions = instructions.scanLeft(Map.empty[String, Int]){case (registers, instruction) =>
  if (instruction.compareOp(registers.getOrElse(instruction.compareReg, 0), instruction.compareAmount)) {
    val oldValue = registers.getOrElse(instruction.reg, 0)
    val newValue = if (instruction.inc) oldValue + instruction.amount else oldValue - instruction.amount
    registers + (instruction.reg -> newValue)
  } else {
    registers
  }
}

val answer1 = runInstructions.last.values.max
println(answer1)

val answer2 = runInstructions.drop(1).map(_.values.max).max
println(answer2)
