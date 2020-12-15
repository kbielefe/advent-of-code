package advent2018

import common._
import monix.eval.Task
import monix.reactive.Observable
import scala.annotation.tailrec

object Day24 extends MultilineStringsDay[Long, Long](2018, 24) {
  private case class Army(
    immuneSystem: Boolean,
    id: Long,
    count: Long,
    hp: Long,
    weaknesses: Set[String],
    immunities: Set[String],
    damage: Long,
    damageType: String,
    initiative: Long) {

    def effectivePower: Long = count * damage

    def boost(extraDamage: Long): Army =
      if (immuneSystem)
        copy(damage = damage + extraDamage)
      else
        this

    def damageDealt(enemy: Army): Long =
      if (enemy.immunities.contains(damageType))
        0
      else if (enemy.weaknesses.contains(damageType))
        effectivePower * 2
      else
        effectivePower

    def applyDamage(damage: Long): Army = {
      copy(count = Math.max(0, count - (damage / hp)))
    }

    def selectDefender(armies: Set[Army]): Option[Army] =
      armies.filter(_.immuneSystem != immuneSystem).filter(damageDealt(_) != 0).maxByOption(enemy => (damageDealt(enemy), enemy.effectivePower, enemy.initiative))
  }

  override def part1(input: Observable[Seq[String]]): Task[Long] =
    input.toListL.map{armies =>
      val immune = parseArmies(armies(0), true)
      val infection = parseArmies(armies(1), false)
      val winner = playUntilWin(immune ++ infection)
      winner.map(_.count).sum
    }

  override def part2(input: Observable[Seq[String]]): Task[Long] =
    input.toListL.map{armies =>
      val immune = parseArmies(armies(0), true)
      val infection = parseArmies(armies(1), false)
      val winner = boostUntilWin(immune ++ infection)
      winner.map(_.count).sum
    }

  @tailrec
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
      val attackOrder = targetSelection.map(_._2).flatten.sortBy(-1 * _._1.initiative).map{case (attacker, defender) => (attacker.immuneSystem, attacker.id, defender.id)}
      val armyMap = armies.map(army => ((army.immuneSystem, army.id) -> army)).toMap
      val newArmyMap = attackOrder.foldLeft(armyMap){case (armyMap, (immuneSystem, attackerId, defenderId)) =>
        val newArmyMap = for {
          attacker <- armyMap.get(immuneSystem -> attackerId)
          defender <- armyMap.get(!immuneSystem -> defenderId)
        } yield {
          val damage = attacker.damageDealt(defender)
          val newDefender = defender.applyDamage(damage)
          if (newDefender.count == 0)
            armyMap - (!immuneSystem -> defenderId)
          else
            armyMap.updated(!immuneSystem -> defenderId, newDefender)
        }
        newArmyMap.getOrElse(armyMap)
      }
      if (armyMap == newArmyMap)
        Set.empty
      else
        playUntilWin(newArmyMap.values.toSet)
    }
  }

  @tailrec
  private def boostUntilWin(armies: Set[Army], min: Long = 0, max: Long = 119): Set[Army] = {
    val boost = (min + max) / 2
    val boostedArmies = armies.map(_.boost(boost))
    val prevResult = playUntilWin(armies.map(_.boost(boost - 1)))
    val result = playUntilWin(armies.map(_.boost(boost)))
    if (result.isEmpty) // stalemate
      boostUntilWin(armies, boost + 1, max)
    else if (max < min)
      throw new Exception(s"Max $max was less than min $min")
    else if (result.head.immuneSystem && (prevResult.isEmpty || !prevResult.head.immuneSystem)) {
      result
    } else if (result.head.immuneSystem)
      boostUntilWin(armies, min, boost)
    else
      boostUntilWin(armies, boost + 1, max)
  }

  private def parseArmies(armyStrings: Seq[String], immuneSystem: Boolean): Set[Army] =
    armyStrings.zipWithIndex.drop(1).map{case (string, index) => parseArmy(immuneSystem, index, string)}.toSet

  private def parseArmy(immuneSystem: Boolean, index: Long, armyString: String): Army = {
    val regex = """(\d+) units each with (\d+) hit points (\(.*\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
    armyString match {
      case regex(count, hp, weak, damage, damageType, initiative) => Army(immuneSystem, index, count.toLong, hp.toLong, parseWeakness(weak), parseImmune(weak), damage.toLong, damageType, initiative.toLong)
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
}
