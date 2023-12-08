package day8
import algorithms.{detectCycle, Mod}
import parse.{*, given}

case class Network(name: String, left: String, right: String)
case class Input(turns: String, network: List[Network] - "\n"):
  def repeatedTurns =
    Iterator.continually(Iterator.from(turns)).flatten

  val networkByName =
    network.map(network => (network.name, network)).toMap

  def path(start: String): Iterator[String] =
    repeatedTurns.scanLeft(start){(name, turn) =>
      if turn == 'L' then networkByName(name).left else networkByName(name).right
    }

given Read[Network] = Read("""(.+) = \((.+), (.+)\)""".r)
type I = Input ~ """(?s)(.+)\n\n(.+)"""

object Puzzle extends runner.Day[I, Int, Long]:
  def part1(input: I): Int =
    input.path("AAA").indexWhere(_ == "ZZZ")

  def part2(input: I): Long =
    val endsWithA = input.network.map(_.name).filter(_.endsWith("A"))
    Mod.lcm(endsWithA.flatMap(start => detectCycle(input.path(start), 1, _.endsWith("Z"))).map(_._2))
