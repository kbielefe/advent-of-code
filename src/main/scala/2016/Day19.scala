package advent2016
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day19:
  def part1(input: Int): Int =
    zipper(elfQueue(1, 1), elfQueue(2, input), 1, input - 1, 1)

  def part2(input: Int): Int =
    val victim = input / 2 + 1
    zipper(elfQueue(1, victim - 1), elfQueue(victim, input), victim - 1, victim, 2)

  private def elfQueue(from: Int, to: Int): Queue[Elf] =
    Queue.range(from, to + 1).map(Elf(_, 1))

  case class Elf(number: Int, presents: Int):
    def steals(victim: Elf): Elf =
      Elf(number, presents + victim.presents)

  @tailrec
  private def zipper(thieves: Queue[Elf], victims: Queue[Elf], thiefCount: Int, victimCount: Int, part: Int): Int =
    val finished = thiefCount == 0 && victimCount == 1
    val unbalanced =
      (part == 2 && thiefCount < (thiefCount + victimCount) / 2) ||
      (part == 1 && thiefCount == 0)

    if finished then
      victims.head.number
    else if unbalanced then
      zipper(thieves.appended(victims.head), victims.tail, thiefCount + 1, victimCount - 1, part)
    else
      zipper(thieves.tail, victims.tail.appended(thieves.head.steals(victims.head)), thiefCount - 1, victimCount, part)
