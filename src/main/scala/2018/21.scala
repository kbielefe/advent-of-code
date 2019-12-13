package advent2018
import common.{DayTask, ElfCode}
import monix.eval.Task
import monix.reactive.Observable

class Day21 extends DayTask[(Int, Vector[Vector[Int]]), Int, String] {

  def stringToOpcode(string: String): Int = string match {
    case "eqir" => 0
    case "borr" => 1
    case "addr" => 2
    case "gtri" => 3
    case "muli" => 4
    case "gtir" => 5
    case "mulr" => 6
    case "banr" => 7
    case "bori" => 8
    case "eqri" => 9
    case "eqrr" => 10
    case "bani" => 11
    case "setr" => 12
    case "gtrr" => 13
    case "addi" => 14
    case "seti" => 15
    case _      => -1
  }

  def reg(num: Int): String = num match {
    case 0 => "a"
    case 1 => "p"
    case 2 => "b"
    case 3 => "c"
    case 4 => "d"
    case 5 => "e"
    case _ => "invalid"
  }

  def simplify(instruction: Vector[Int]): String = instruction match {
    case Vector(0, a, b, c) => s"eqir $a ${reg(b)} ${reg(c)}"
    case Vector(1, a, b, c) => s"borr ${reg(a)} ${reg(b)} ${reg(c)}"
    case Vector(2, a, b, c) => s"addr ${reg(a)} ${reg(b)} ${reg(c)}"
    case Vector(3, a, b, c) => s"gtri ${reg(a)} $b ${reg(c)}"
    case Vector(4, a, b, c) => s"muli ${reg(a)} $b ${reg(c)}"
    case Vector(5, a, b, c) => s"gtir $a ${reg(b)} ${reg(c)}"
    case Vector(6, a, b, c) => s"mulr ${reg(a)} ${reg(b)} ${reg(c)}"
    case Vector(7, a, b, c) => s"banr ${reg(a)} ${reg(b)} ${reg(c)}"
    case Vector(8, a, b, c) => s"bori ${reg(a)} $b ${reg(c)}"
    case Vector(9, a, b, c) => s"eqri ${reg(a)} $b ${reg(c)}"
    case Vector(10, a, b, c) => s"eqrr ${reg(a)} ${reg(b)} ${reg(c)}"
    case Vector(11, a, b, c) => s"bani ${reg(a)} $b ${reg(c)}"
    case Vector(12, a, _, c) => s"setr ${reg(a)} ${reg(c)}"
    case Vector(13, a, b, c) => s"gtrr ${reg(a)} ${reg(b)} ${reg(c)}"
    case Vector(14, a, b, c) => s"addi ${reg(a)} $b ${reg(c)}"
    case Vector(15, a, _, c) => s"seti $a ${reg(c)}"
    case _      => "unknown instruction"
  }

  override def input(lines: Observable[String]) = {
    val ip = lines.headL.map(_.split(' ')(1).toInt)
    val instructions = lines.drop(1).map(_.split(' ')).map{
      case Array(opcode, a, b, c) => Vector(stringToOpcode(opcode), a.toInt, b.toInt, c.toInt)
    }
    val instructionsTask = instructions.toListL.map(_.toVector)
    for {
      x <- ip
      y <- instructionsTask
    } yield (x, y)
  }

  override def part1(input: (Int, Vector[Vector[Int]])) = Task{
    ElfCode.executeWithJumps(input._1, input._2, Vector(3941014, 0, 0, 0, 0, 0))(0)
  }

  override def part2(input: (Int, Vector[Vector[Int]])) = Task{
    "unimplemented"
  }
}
