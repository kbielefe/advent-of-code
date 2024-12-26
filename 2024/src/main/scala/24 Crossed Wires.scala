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

  def swap(left: String, right: String): Gate =
    def helper(value: String): String =
      if value == left then
        right
      else if value == right then
        left
      else
        value
    copy(output = helper(output))

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

  def containsLoop(visited: Set[String]): Boolean =
    val inputs = signals.filter(_.startsWith("z")).flatMap:signal =>
      val gate = gates.find(_.output == signal).get
      List(gate.lhs, gate.rhs)
    ???

  def allCorrect(swaps: List[(String, String)]): Boolean =
    val input = swaps.foldLeft(this){case (input, (left, right)) => input.swap(left, right)}
    !input.containsLoop(Set.empty) &&
    input.signals.filter(_.startsWith("z")).forall(signal => input.swaps(signal).isEmpty)

  def swap(left: String, right: String): Input =
    copy(gates = gates.map(_.swap(left, right)))

  def swaps(signal: String): Set[(String, String)] =
    zeroInputs.expectedSwap(false, signal) ++
    zeroInputs.withOne("x" + signal.drop(1)).expectedSwap(true, signal) ++
    zeroInputs.withOne("y" + signal.drop(1)).expectedSwap(true, signal) ++
    zeroInputs.withOne("x" + signal.drop(1)).withOne("y" + signal.drop(1)).expectedSwap(false, signal)

  def expectedSwap(expected: Boolean, signal: String): Set[(String, String)] =
    if calculate(signal) == expected then
      Set.empty
    else
      val (ones, zeros) = allOutputs(signal)
        .filterNot(output => "xyz".contains(output.head))
        .partition(calculate)
      val pairs = for
        one  <- ones
        zero <- zeros
      yield one -> zero
      pairs.filter((left, right) => swap(left, right).calculate(signal) == expected)
        .map((l, r) => if l < r then (l, r) else (r, l))

given Read[Gate] = Read("""(\S+) (\S+) (\S+) -> (\S+)""".r)
given Read[List[Gate]] = Read("\n")
given Read[(String, Int)] = Read[(String, Int)](": ")
given Read[Map[String, Int]] = Read[List, (String, Int)]("\n").map(_.toMap)
given Read[Input] = Read("\n\n")

object Puzzle extends runner.Day[Input, Long, String]:
  def part1(input: Input): Long =
    input.signals.filter(_.startsWith("z")).toList.sorted.reverse.map(input.calculate).map(bool => if bool then 1L else 0L).reduceLeft(_ * 2 + _)

  def part2(input: Input): String =
    val result = input.signals.filter(_.startsWith("z")).flatMap(input.swaps)
    val actualResult = result.toList.combinations(4).find(input.allCorrect).get.map((l, r) => List(l, r)).sorted
    assert(actualResult.size == 8, "Must be 8 swaps")
    actualResult.mkString(",")
