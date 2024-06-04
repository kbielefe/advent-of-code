package day13
import parse.{*, given}

case class Happiness(guest: String, gainOrLose: String, amount: Int, neighbor: String)

given Read[Happiness] = Read("""(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.""".r)
given Read[List[Happiness]] = Read("\n")

object Puzzle extends runner.Day[List[Happiness], Int, Int]:
  def part1(input: List[Happiness]): Int =
    val happinessByGuest = input.groupBy(_.guest).view.mapValues(_.map(happiness => happiness.neighbor -> (if happiness.gainOrLose == "lose" then -happiness.amount else happiness.amount)).toMap).toMap
    happinessByGuest.keys.toVector.permutations.map(totalHappiness(happinessByGuest)).max

  def part2(input: List[Happiness]): Int =
    val happinessByGuest = input.groupBy(_.guest).view.mapValues(_.map(happiness => happiness.neighbor -> (if happiness.gainOrLose == "lose" then -happiness.amount else happiness.amount)).toMap).toMap
    (happinessByGuest.keySet + "me").toVector.permutations.map(totalHappiness(happinessByGuest)).max

  def totalHappiness(happinessByGuest: Map[String, Map[String, Int]])(arrangement: Vector[String]): Int =
    arrangement.zipWithIndex.map{(guest, index) =>
      val leftNeighbor = if index == 0 then arrangement.last else arrangement(index - 1)
      val rightNeighbor = if (index + 1) == arrangement.size then arrangement.head else arrangement(index + 1)
      happinessByGuest.getOrElse(guest, Map.empty).getOrElse(leftNeighbor, 0) +
      happinessByGuest.getOrElse(guest, Map.empty).getOrElse(rightNeighbor, 0)
    }.sum
