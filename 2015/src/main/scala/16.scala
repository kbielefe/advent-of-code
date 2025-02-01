package day16
import parse.{*, given}

val gift = List(
  Item("children", 3),
  Item("cats", 7),
  Item("samoyeds", 2),
  Item("pomeranians", 3),
  Item("akitas", 0),
  Item("vizslas", 0),
  Item("goldfish", 5),
  Item("trees", 3),
  Item("cars", 2),
  Item("perfumes", 1)
)

case class Item(name: String, count: Int):
  def matches(other: Item): Boolean = name match
    case "cats" | "trees" => count > other.count
    case "pomeranians" | "goldfish" => count < other.count
    case _ => count == other.count

case class Sue(number: Int, items: List[Item]):
  def boughtGift: Boolean =
    gift.forall(giftItem => items.find(_.name == giftItem.name).map(_.count == giftItem.count).getOrElse(true))
  def reallyBoughtGift: Boolean =
    gift.forall(giftItem => items.find(_.name == giftItem.name).map(_ `matches` giftItem).getOrElse(true))

given Read[Item] = Read("""(\w+): (\d+)""".r)
given Read[Sue] = Read("""Sue (\d+): (.+)""".r)
given li: Read[List[Item]] = Read(", ")
given ls: Read[List[Sue]] = Read("\n")

object Puzzle extends runner.Day[List[Sue], Int, Int]:
  def part1(input: List[Sue]): Int =
    input.find(_.boughtGift).get.number

  def part2(input: List[Sue]): Int =
    input.find(_.reallyBoughtGift).get.number
