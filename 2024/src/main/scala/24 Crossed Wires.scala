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

  def allOutputs(signal: String): Set[String] =
    Set(signal) ++ gates.find(_.output == signal).map(gate => allOutputs(gate.lhs) ++ allOutputs(gate.rhs)).getOrElse(Set.empty)

  def zeroInputs: Input =
    Input(initial.view.mapValues(_ => 0).toMap, gates)

  def withOne(signal: String): Input =
    Input(initial + (signal -> 1), gates)

  def isCorrect(signal: String): Boolean =
    val num = signal.drop(1)
    val carryCorrect =
      num == "00" || {
        val prev = "%02d".format(num.toInt - 1)
        zeroInputs.withOne("x" + prev).withOne("y" + prev).calculate(signal)
      }
    carryCorrect &&
    !zeroInputs.calculate(signal) &&
    zeroInputs.withOne("x" + num).calculate(signal) &&
    zeroInputs.withOne("y" + num).calculate(signal) &&
    !zeroInputs.withOne("x" + num).withOne("y" + num).calculate(signal)

  def whyIncorrect(signal: String): String =
    val num = signal.drop(1)
    val carryCorrect = num == "00" || {
      val prev = "%02d".format(num.toInt - 1)
      zeroInputs.withOne("x" + prev).withOne("y" + prev).calculate(signal)
    }
    s"""$signal
    |carry   $carryCorrect
    |both 0  ${!zeroInputs.calculate(signal)}
    |both 1  ${!zeroInputs.withOne("x" + num).withOne("y" + num).calculate(signal)}
    |x$num = 1 ${zeroInputs.withOne("x" + num).calculate(signal)}
    |y$num = 1 ${zeroInputs.withOne("y" + num).calculate(signal)}
    """.stripMargin

given Read[Gate] = Read("""(\S+) (\S+) (\S+) -> (\S+)""".r)
given Read[List[Gate]] = Read("\n")
given Read[(String, Int)] = Read[(String, Int)](": ")
given Read[Map[String, Int]] = Read[List, (String, Int)]("\n").map(_.toMap)
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Long, String]:
  def part1(input: Input): Long =
    input.signals.filter(_.startsWith("z")).toList.sorted.reverse.map(input.calculate).map(bool => if bool then 1L else 0L).reduceLeft(_ * 2 + _)

  def part2(input: Input): String =
    input.signals.filter(_.startsWith("z")).toList.sorted.map(signal => input.allOutputs(signal)).sliding(2).map{case Seq(l, r) => (r -- l).toList.sorted.mkString(",")}.foreach(println)
    ???
