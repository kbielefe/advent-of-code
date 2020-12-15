package advent2018

import common._
import monix.eval.Task
import monix.reactive.Observable
import scala.annotation.tailrec

object Day24 extends MultilineStringsDay[Int, Int](2018, 24) {
  private case class Army(immuneSystem: Boolean, id: Int, count: Int, hp: Int, weaknesses: Set[String], immunities: Set[String], damage: Int, damageType: String, initiative: Int) {
    def effectivePower: Int = count * damage

    def damageDealt(enemy: Army): Int =
      if (enemy.immunities.contains(damageType))
        0
      else if (enemy.weaknesses.contains(damageType))
        effectivePower * 2
      else
        effectivePower

    def selectDefender(armies: Set[Army]): Option[Army] =
      armies.filter(_.immuneSystem != immuneSystem).filter(damageDealt(_) != 0).maxByOption(enemy => (damageDealt(enemy), enemy.effectivePower, enemy.initiative))
  }

  override def part1(input: Observable[Seq[String]]): Task[Int] =
    input.toListL.map{armies =>
      val immune = parseArmies(armies(0), true)
      val infection = parseArmies(armies(1), false)
      val winner = playUntilWin(immune ++ infection)
      winner.map(_.count).sum
    }

  override def part2(input: Observable[Seq[String]]): Task[Int] =
    ???


  private def playUntilWin(armies: Set[Army]): Set[Army] = {
    if (armies.filter(_.immuneSystem).map(_.count).sum == 0) {
      armies.filterNot(_.immuneSystem)
    } else if (armies.filterNot(_.immuneSystem).map(_.count).sum == 0) {
      armies.filter(_.immuneSystem)
    } else {
      val targetSelectionOrder = armies.toList.sortBy(army => (-1 * army.effectivePower, -1 * army.initiative))
      val targetSelection = targetSelectionOrder.scanLeft[(Set[Army], Option[(Army, Army)])]((armies, None)){case ((armies, _), attacker) =>
        attacker
          .selectDefender(armies)
          .map(defender => (armies - defender, Some(attacker -> defender)))
          .getOrElse((armies, None))
      }
      val attackOrder = targetSelection.map(_._2).flatten.sortBy(-1 * _._1.initiative)
      attackOrder.foldLeft(armies){case (armies, (attacker, defender)) =>
        // TODO: Start here. Probably want to use a map instead of a set of
        // armies to speed up lookups as changes are made during the round.
        ???
      }
      ???
    }
  }

  private def parseArmies(armyStrings: Seq[String], immuneSystem: Boolean): Set[Army] =
    armyStrings.zipWithIndex.drop(1).map{case (string, index) => parseArmy(immuneSystem, index, string)}.toSet

  private def parseArmy(immuneSystem: Boolean, index: Int, armyString: String): Army = {
    val regex = """(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
    armyString match {
      case regex(count, hp, weak, damage, damageType, initiative) => Army(immuneSystem, index, count.toInt, hp.toInt, parseWeakness(weak), parseImmune(weak), damage.toInt, damageType, initiative.toInt)
    }
  }

  private def parseWeakness(input: String): Set[String] = {
    val regex = """weak to ([^);]+)""".r.unanchored
    input match {
      case regex(weaknesses) => weaknesses.split(", ").toSet
      case _ => Set.empty
    }
  }

  private def parseImmune(input: String): Set[String] = {
    val regex = """immune to ([^);]+)""".r.unanchored
    input match {
      case regex(immunities) => immunities.split(", ").toSet
      case _ => Set.empty
    }
  }
    /*
 Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4 }
  */
}
