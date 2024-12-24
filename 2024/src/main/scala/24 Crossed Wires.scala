package day24
import parse.{*, given}

case class Gate(lhs: String, kind: String, rhs: String, output: String):
  def signals: Set[String] =
    Set(lhs, rhs, output)

  def calculate(input: Input): Boolean = kind match
    case "AND" => input.calculate(lhs) && input.calculate(rhs)
    case "OR" => input.calculate(lhs) || input.calculate(rhs)
    case "XOR" => input.calculate(lhs) ^ input.calculate(rhs)
    case _ => ???

case class Input(initial: Map[String, Int], gates: List[Gate]):
  def signals: Set[String] =
    gates.flatMap(_.signals).toSet

  def calculate(signal: String): Boolean =
    initial.get(signal).map(_ == 1)
      .orElse(gates.find(_.output == signal).map(_.calculate(this))).get

given Read[Gate] = Read("""(\S+) (\S+) (\S+) -> (\S+)""".r)
given Read[List[Gate]] = Read("\n")
given Read[(String, Int)] = Read[(String, Int)](": ")
given Read[Map[String, Int]] = Read[List, (String, Int)]("\n").map(_.toMap)
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Long, String]:
  def part1(input: Input): Long =
    input.signals.filter(_.startsWith("z")).toList.sorted.reverse.map(input.calculate).map(bool => if bool then 1L else 0L).reduceLeft(_ * 2 + _)

  def part2(input: Input): String =
    println(input.gates.size)
    ???
