package day13
import parse.{*, given}

case class Button(x: Long, y: Long)
case class Prize(x: Long, y: Long)
case class ClawMachine(a: Button, b: Button, prize: Prize):
  def tokens(offset: Long): Option[Long] =
    val aNum = (prize.x + offset) * b.y - (prize.y + offset) * b.x
    val aDenom = a.x * b.y - a.y * b.x
    Option.when(aNum % aDenom == 0)(aNum / aDenom).flatMap: aPresses =>
      val bNum = (prize.x + offset) - aPresses * a.x
      val bDenom = b.x
      Option.when(bNum % bDenom == 0)(3 * aPresses + bNum / bDenom)

given Read[Button] = Read("""Button .: X\+(\d+), Y\+(\d+)""".r)
given Read[Prize] = Read("""Prize: X=(\d+), Y=(\d+)""".r)
given Read[ClawMachine] = Read("\n")
given Read[List[ClawMachine]] = Read("\n\n")

object Puzzle extends runner.Day[List[ClawMachine], Long, Long]:
  def part1(clawMachines: List[ClawMachine]): Long =
    clawMachines.flatMap(_.tokens(0)).sum

  def part2(clawMachines: List[ClawMachine]): Long =
    clawMachines.flatMap(_.tokens(10000000000000L)).sum
