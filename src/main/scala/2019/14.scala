package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable

// TODO: Properly report error parsing input
class Day14 extends DayTask[Map[String, (Int, List[(Int, String)])], Int, String] {

  type DAG = Map[String, (Int, List[(Int, String)])]

  override def input(lines: Observable[String]) = lines.map{line =>
    val Array(sources, target) = line.split(" => ")
    val parsedSources = sources.split(", ").map{source =>
      val Array(sourceCountString, sourceName) = source.split(" ")
      (sourceCountString.toInt, sourceName)
    }
    val Array(targetCountString, targetName) = target.split(" ")
    (targetName -> (targetCountString.toInt, parsedSources.toList))
  }.toListL.map(_.toMap)

  def noIncomingEdges(reactions: DAG): Set[String] = {
    val haveIncoming = reactions.values.map(_._2).flatMap(_.map(_._2)).toSet
    val allNodes = reactions.keySet
    allNodes -- haveIncoming
  }

  def updateDemanded(demanded: Map[String, Int], reactants: List[(Int, String)], batches: Int): Map[String, Int] = {
    reactants.foldLeft(demanded){case (demanded, (count, name)) =>
      val oldCount = demanded.getOrElse(name, 0)
      demanded.updated(name, count * batches + oldCount)
    }
  }

  def removeNode(reactions: DAG, node: String): DAG = {
    reactions - node
  }

  @scala.annotation.tailrec
  final def oreCount(reactions: DAG, demanded: Map[String, Int]): Int = {
    val noIncoming = noIncomingEdges(reactions)
    if (noIncoming.isEmpty) {
      demanded("ORE")
    } else {
      val current = noIncoming.head
      val (count, reactants) = reactions(current)
      val batches = if (demanded(current) % count == 0) demanded(current) / count else demanded(current) / count + 1
      val newDemanded = updateDemanded(demanded, reactants, batches)
      val newReactions = removeNode(reactions, current)
      oreCount(newReactions, newDemanded)
    }
  }

  override def part1(reactions: DAG) = Task{oreCount(reactions, Map("FUEL" -> 1))}

  override def part2(reactions: DAG) = Task{"unimplemented"}
}
