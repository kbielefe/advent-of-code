package advent2018
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

sealed trait Team
case object Immune extends Team
case object Infection extends Team

case class Army(
   team: Team,
   count: Int,
   hp: Int,
   weaknesses: List[String],
   immunities: List[String],
   damage: Int,
   damageType: String,
   initiative: Int) {
  def effectivePower = count * damage
}

class Day24 extends DayTask[List[Army], Int, String] {

  override def input(lines: Observable[String]) = Task{List(
    Army(Immune,    357,  6038,  List("bludgeoning"),             List.empty,                          166, "slashing",     5),
    Army(Immune,    357,  6038,  List("bludgeoning"),             List.empty,                          166, "slashing",     5),
    Army(Immune,    1265, 3299,  List("cold"),                    List("radiation"),                   25,  "fire",         3),
    Army(Immune,    137,  3682,  List.empty,                      List.empty,                          264, "radiation",   13),
    Army(Immune,    5484, 2545,  List.empty,                      List("slashing"),                    4,   "bludgeoning", 16),
    Army(Immune,    5242, 11658, List("radiation"),               List.empty,                          19,  "bludgeoning", 20),
    Army(Immune,    3333, 7277,  List("slashing", "bludgeoning"), List.empty,                          16,  "slashing",     4),
    Army(Immune,    6136, 5157,  List("radiation"),               List("fire"),                        7,   "fire",        14),
    Army(Immune,    1215, 2154,  List.empty,                      List.empty,                          13,  "bludgeoning", 15),
    Army(Immune,    753,  3242,  List("slashing"),                List.empty,                          33,  "cold",         6),
    Army(Immune,    1325, 8064,  List.empty,                      List.empty,                          58,  "cold",         2),
    Army(Infection, 812,  13384, List("cold"),                    List("slashing"),                    29,  "fire",         7),
    Army(Infection, 971,  28820, List.empty,                      List("fire"),                        55,  "bludgeoning",  8),
    Army(Infection, 159,  51016, List("slashing"),                List.empty,                          482, "radiation",   11),
    Army(Infection, 5813, 10079, List.empty,                      List("bludgeoning", "cold", "fire"), 2,   "cold",        18),
    Army(Infection, 708,  30474, List.empty,                      List.empty,                          76,  "bludgeoning", 10),
    Army(Infection, 874,  39547, List.empty,                      List("radiation", "fire", "cold"),   69,  "radiation",   12),
    Army(Infection, 1608, 30732, List("fire", "cold"),            List.empty,                          29,  "radiation",   17),
    Army(Infection, 6715, 14476, List("radiation"),               List.empty,                          3,   "bludgeoning", 19),
    Army(Infection, 2023, 36917, List("slashing"),                List.empty,                          36,  "bludgeoning",  1),
    Army(Infection, 1214, 14587, List.empty,                      List("cold"),                        16,  "radiation",    9)
  )}

  def fight(armies: List[Army]): List[Army] = {
    val selectionOrder = armies.sortBy(army => (army.effectivePower * -1, army.initiative * -1))
    ???
  }

  def battle(armies: List[Army]): List[Army] = {
    Iterator.iterate(armies)(fight).dropWhile(bothTeamsSurvive).next()
  }

  def bothTeamsSurvive(armies: List[Army]): Boolean = {
    armies.map(_.team).toSet.size > 1
  }

  override def part1(armies: List[Army]) = Task{
    0
  }

  override def part2(armies: List[Army]) = Task{
    "unimplemented"
  }
}
