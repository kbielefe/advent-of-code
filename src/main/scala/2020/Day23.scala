package advent2020

object Day23:
  def part1(input: String): String =
    val cups = input.trim.toVector.map(_.asDigit)
    orderAfter1(Iterator.iterate(cups)(move).drop(100).next)

  def part2(input: Int): Int =
    ???

  private def move(cups: Vector[Int]): Vector[Int] =
    val pickedUp = cups.slice(1, 4)
    val withoutPickedUp = cups.head +: cups.drop(4)
    val destination = ((cups.head - 1) to (cups.head - 5) by -1).iterator.map(x => if x <= 0 then x + 9 else x).find(x => !pickedUp.contains(x)).get
    val destinationIndex = withoutPickedUp.indexOf(destination)
    val (before, after) = withoutPickedUp.splitAt(destinationIndex + 1)
    val inserted = before ++ pickedUp ++ after
    inserted.tail :+ inserted.head

  private def orderAfter1(cups: Vector[Int]): String =
    val (before, after) = cups.splitAt(cups.indexOf(1))
    (after.tail ++ before).mkString
