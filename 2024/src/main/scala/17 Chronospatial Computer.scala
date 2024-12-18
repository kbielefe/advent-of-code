package day17
import parse.{*, given}
import scala.annotation.tailrec

type Register = Long
case class Registers(a: Register, b: Register, c: Register)
case class Computer(registers: Registers, program: Vector[Long]):
  def run: List[Long] =
    run(0, registers, List.empty)

  def withA(a: Long): Computer =
    copy(registers = registers.copy(a = a))

  def combo(ip: Int, reg: Registers): Long = program(ip + 1) match
    case 0 | 1 | 2 | 3 => program(ip + 1)
    case 4 => reg.a
    case 5 => reg.b
    case 6 => reg.c
    case 7 => throw new Exception("reserved")

  def literal(ip: Int): Int = program(ip + 1).toInt

  @tailrec
  private def run(ip: Int, reg: Registers, output: List[Long]): List[Long] =
    if ip >= program.size then
      output.reverse
    else program(ip) match
      case 0 => run(ip + 2, reg.copy(a = reg.a >> combo(ip, reg)), output)
      case 1 => run(ip + 2, reg.copy(b = reg.b ^ literal(ip)), output)
      case 2 => run(ip + 2, reg.copy(b = combo(ip, reg) % 8), output)
      case 3 => run(if reg.a == 0 then ip + 2 else literal(ip), reg, output)
      case 4 => run(ip + 2, reg.copy(b = reg.b ^ reg.c), output)
      case 5 => run(ip + 2, reg, (combo(ip, reg) % 8) :: output)
      case 6 => run(ip + 2, reg.copy(b = reg.a >> combo(ip, reg)), output)
      case 7 => run(ip + 2, reg.copy(c = reg.a >> combo(ip, reg)), output)

given Read[Register] = summon[Read[String]].map(_.split(": ")(1).toInt)
given Read[Registers] = Read("\n")
given Read[Vector[Long]] = summon[Read[String]].map(_.split(": ")(1).split(",").toVector.map(_.toLong))
given Read[Computer] = Read("\n\n")

object Puzzle extends runner.Day[Computer, String, Long]:
  def part1(computer: Computer): String =
    computer.run.mkString(",")

  def part2(computer: Computer): Long =
    findQuine(computer, computer.program.reverse.toList, 0).get

  def findQuine(computer: Computer, output: List[Long], a: Long): Option[Long] =
    if output.isEmpty then
      Some(a)
    else
      (0 to 7)
        .map(_ + (a << 3))
        .iterator
        .filter(a => computer.withA(a).run.head == output.head)
        .flatMap(findQuine(computer, output.tail, _))
        .nextOption
