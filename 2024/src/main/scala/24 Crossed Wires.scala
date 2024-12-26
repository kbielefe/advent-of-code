package day24
import parse.{*, given}

case class Gate(lhs: String, kind: String, rhs: String, output: String):
  def calculate(input: Input): Boolean = kind match
    case "AND" => input.calculate(lhs) && input.calculate(rhs)
    case "OR" => input.calculate(lhs) || input.calculate(rhs)
    case "XOR" => input.calculate(lhs) ^ input.calculate(rhs)
    case _ => ???

  override def toString: String =
    s"$lhs $kind $rhs == $output"

  def swap(l: String, r: String): Gate =
    copy(output = if output == l then r else if output == r then l else output)

case class Input(initial: Map[String, Int], gates: List[Gate]):
  def calculate(signal: String): Boolean =
    initial.get(signal).map(_ == 1)
      .orElse(gates.find(_.output == signal).map(_.calculate(this))).get

  def swap(lhs: String, rhs: String): Input =
    copy(gates = gates.map(_.swap(lhs, rhs)))

  def x(num: Int): Traversal =
    new Signal("x%02d".format(num))

  trait Traversal:
    infix def and(char: Char): Traversal
    infix def or(char: Char): Traversal
    infix def xor(char: Char): Traversal
    infix def xor(num: Int): List[String]

  object Empty extends Traversal:
    infix def and(char: Char): Traversal = Empty
    infix def or(char: Char): Traversal = Empty
    infix def xor(char: Char): Traversal = Empty
    infix def xor(num: Int): List[String] = List.empty

  class Signal(signal: String, accum: List[String] = List.empty) extends Traversal:
    infix def and(char: Char): Traversal =
      gates.find(gate => gate.kind == "AND" && (gate.lhs == signal || gate.rhs == signal)) match
        case Some(gate) => new Signal(gate.output, gate.output :: accum)
        case None =>
          println(s"Couldn't find $signal AND _ == $char")
          Empty

    infix def or(char: Char): Traversal =
      gates.find(gate => gate.kind == "OR" && (gate.lhs == signal || gate.rhs == signal)) match
        case Some(gate) => new Signal(gate.output, gate.output :: accum)
        case None =>
          println(s"Couldn't find $signal OR _ == $char")
          Empty

    infix def xor(char: Char): Traversal =
      gates.find(gate => gate.kind == "XOR" && (gate.lhs == signal || gate.rhs == signal)) match
        case Some(gate) => new Signal(gate.output, gate.output :: accum)
        case None =>
          println(s"Couldn't find $signal XOR _ == $char")
          Empty

    infix def xor(num: Int): List[String] =
      gates.find(gate => gate.kind == "XOR" && (gate.lhs == signal || gate.rhs == signal) && gate.output == "z%02d".format(num)) match
        case Some(gate) => accum.reverse
        case None =>
          println(s"Couldn't find $signal XOR _ == ${"z%02d".format(num)}")
          println(s"Found ${gates.filter(gate => gate.output == "z%02d".format(num) || gate.lhs == signal || gate.rhs == signal || gate.output == signal).mkString(", ")}")
          List.empty

given Read[Gate] = Read("""(\S+) (\S+) (\S+) -> (\S+)""".r)
given Read[List[Gate]] = Read("\n")
given Read[(String, Int)] = Read[(String, Int)](": ")
given Read[Map[String, Int]] = Read[List, (String, Int)]("\n").map(_.toMap)
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Long, String]:
  def part1(input: Input): Long =
    input.gates.map(_.output).filter(_.startsWith("z")).toList.sorted.reverse.map(input.calculate).map(bool => if bool then 1L else 0L).reduceLeft(_ * 2 + _)

  def part2(input: Input): String =
    val swapped = input
      .swap("wjb", "cvp") // Correct
      .swap("z34", "wcb") // Correct
      .swap("z14", "qbw") // Correct
      .swap("z10", "mkk") // Correct
    (1 to 43).foreach: num =>
      println(num)
      println("a")
      val a = swapped.x(num) xor 'A' xor num
      println("acd")
      val acd = swapped.x(num) xor 'A' and 'C' or 'D' xor (num + 1)
      println("bd")
      val bd = swapped.x(num) and 'B' or 'D' xor (num + 1)
      println()
      a.size == 1 && acd.size == 3 && bd.size == 2 &&
      a(0) == acd(0) &&
      acd(2) == bd(1)
    // Found by manual inspection of the above output and guessing swaps with nearby signals
    "cvp,mkk,qbw,wcb,wjb,z10,z14,z34"
