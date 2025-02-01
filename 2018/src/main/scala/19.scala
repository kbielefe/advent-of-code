package advent2018
import common.{DayTask, ElfCode}
import monix.eval.Task
import monix.reactive.Observable

class Day19 extends DayTask[(Int, Vector[Vector[Int]]), Int, String] {

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
    ElfCode.executeWithJumps(input._1, input._2)(0)
  }

  override def part2(input: (Int, Vector[Vector[Int]])) = Task{
    "Manually calculate all factors of register 2 when pc = 35, and sum"
    //ElfCode.executeWithJumps(input._1, input._2, Vector(1, 0, 0, 0, 0, 0))(0)
  }
}
