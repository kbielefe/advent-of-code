package advent2019
import common.DayTask
import monix.eval.Task
import monix.reactive.Observable
import scala.annotation.tailrec

class Day14 extends DayTask[Map[String, (Long, List[(Long, String)])], Long, Long] {

  type DAG = Map[String, (Long, List[(Long, String)])]

  override def input(lines: Observable[String]) = lines.map{line =>
    val Array(sources, target) = line.split(" => ")
    val parsedSources = sources.split(", ").map{source =>
      val Array(sourceCountString, sourceName) = source.split(" ")
      (sourceCountString.toLong, sourceName)
    }
    val Array(targetCountString, targetName) = target.split(" ")
    (targetName -> (targetCountString.toLong, parsedSources.toList))
  }.toListL.map(_.toMap)

  def noIncomingEdges(reactions: DAG): Set[String] = {
    val haveIncoming = reactions.values.map(_._2).flatMap(_.map(_._2)).toSet
    val allNodes = reactions.keySet
    allNodes -- haveIncoming
  }

  def updateDemanded(demanded: Map[String, Long], reactants: List[(Long, String)], batches: Long): Map[String, Long] = {
    reactants.foldLeft(demanded){case (demanded, (count, name)) =>
      val oldCount = demanded.getOrElse(name, 0L)
      demanded.updated(name, count * batches + oldCount)
    }
  }

  def removeNode(reactions: DAG, node: String): DAG = {
    reactions - node
  }

  @tailrec
  final def oreCount(reactions: DAG, demanded: Map[String, Long]): Long = {
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

  @tailrec
  final def binarySearch(f: Long => Long, target: Long, min: Long, max: Long): Long = {
    if (min >= max) {
      if (f(min) > target) {
        min - 1
      } else {
        min
      }
    } else {
      val current = (max - min) / 2L + min
      val (newMin, newMax) = if (f(current) > target) {
        (min, current - 1)
      } else {
        (current + 1, max)
      }
      binarySearch(f, target, newMin, newMax)
    }
  }

  override def part1(reactions: DAG) = Task{oreCount(reactions, Map("FUEL" -> 1))}

  override def part2(reactions: DAG) = Task{binarySearch(fuel => oreCount(reactions, Map("FUEL" -> fuel)), 1000000000000L, 0L, 10000000L)}
}
