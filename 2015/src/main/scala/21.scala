package day21
import parse.{*, given}
import scala.annotation.tailrec

case class Stats(hp: Int, damage: Int, armor: Int)
given Read[Stats] = Read("""\D+(\d+)\n\D+(\d+)\n\D+(\d+)""".r)

case class Item(name: String, cost: Int, damage: Int, armor: Int)

val weapons = Set(
  Item("Dagger",      8, 4, 0),
  Item("Shortsword", 10, 5, 0),
  Item("Warhammer",  25, 6, 0),
  Item("Longsword",  40, 7, 0),
  Item("Greataxe",   74, 8, 0)
)

val armor = Set(
  Item("Leather",     13, 0, 1),
  Item("Chainmail",   31, 0, 2),
  Item("Splintmail",  53, 0, 3),
  Item("Bandedmail",  75, 0, 4),
  Item("Platemail",  102, 0, 5),
  Item("No armor", 0, 0, 0)
)

val rings = Set(
  Item("Damage +1",  25, 1, 0),
  Item("Damage +2",  50, 2, 0),
  Item("Damage +3", 100, 3, 0),
  Item("Defense +1", 20, 0, 1),
  Item("Defense +2", 40, 0, 2),
  Item("Defense +3", 80, 0, 3),
  Item("No ring 1", 0, 0, 0),
  Item("No ring 2", 0, 0, 0)
)

def itemCombinations: Iterator[Set[Item]] = for
  weapon <- weapons.iterator
  armor  <- armor.iterator
  ring1  <- rings.iterator
  ring2  <- (rings - ring1).iterator
yield Set(weapon, armor, ring1, ring2)

object Puzzle extends runner.Day[Stats, Int, Int]:
  def part1(bossStats: Stats): Int =
    itemCombinations.filter(wins(bossStats)).map(cost).min

  def part2(bossStats: Stats): Int =
    itemCombinations.filterNot(wins(bossStats)).map(cost).max

  def wins(bossStats: Stats)(items: Set[Item]): Boolean =
    val myStats = Stats(100, items.toList.map(_.damage).sum, items.toList.map(_.armor).sum)
    play(true, myStats, bossStats)

  def cost(items: Set[Item]): Int =
    items.toList.map(_.cost).sum

  @tailrec
  def play(myAttack: Boolean, attacker: Stats, defender: Stats): Boolean =
    val damageTaken = math.max(1, attacker.damage - defender.armor)
    val newHp = defender.hp - damageTaken
    if newHp <= 0 then
      myAttack
    else
      play(!myAttack, defender.copy(hp = newHp), attacker)
