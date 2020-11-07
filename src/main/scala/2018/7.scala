package advent2018
import common.Day
import scala.io.Source
import scala.annotation.tailrec

class Day7(source: Source, offset: Int = 61, workerCount: Int = 5) extends Day {
  val input = source.getLines().toList

  type Order = (Char, Char)

  val inputLine = """Step (.) must be finished before step (.) can begin.""".r

  def parseLine(line: String): Order = line match {
    case inputLine(before, after) => (before.head, after.head)
  }

  val orders = input map parseLine
  val edges = orders.foldLeft(Map.empty[Char, Set[Char]]){case (accum, (before, after)) =>
    val prev = accum.getOrElse(before, Set.empty[Char])
    accum + ((before, prev + after))
  }

  def hasIncoming(edges: Map[Char, Set[Char]]): Set[Char] = {
    edges.values.foldLeft(Set.empty[Char]){case (accum, next) => accum ++ next}
  }

  val nodes = edges.keySet ++ hasIncoming(edges)

  def getNoIncoming(edges: Map[Char, Set[Char]], nodes: Set[Char]): Set[Char] = {
    nodes -- hasIncoming(edges)
  }

  def duration(task: Char): Int = task - 'A' + offset

  @tailrec
  private def toposort(workerCount: Int, result: List[Char], totalDuration: Int, noIncoming: Set[Char], edges: Map[Char, Set[Char]], nodes: Set[Char], wip: Set[(Char, Int)]): (String, Int) = {
    if (noIncoming.isEmpty)
      (result.reverse.mkString(""), totalDuration - 1)
    else {
      val newDuration = totalDuration + 1
      val (carryoverWip, completed) = wip map {case (task, remaining) => (task, remaining - 1)} partition {_._2 > 0}
      val completedTasks = completed map {_._1}
      val carryoverTasks = carryoverWip map {_._1}
      val newNodes = nodes -- completedTasks
      val newEdges = edges -- completedTasks
      val newNoIncoming = getNoIncoming(newEdges, newNodes)
      val newResult = completedTasks.toList ++ result

      val availableWorkers = workerCount - carryoverWip.size
      val newTasks = (newNoIncoming -- carryoverTasks).toList.sorted.take(availableWorkers).toSet

      val newWip = carryoverWip ++ (newTasks map {task => (task, duration(task))})
      toposort(workerCount, newResult, newDuration, newNoIncoming, newEdges, newNodes, newWip)
    }
  }

  override def answer1 = (toposort(1, List.empty[Char], 0, getNoIncoming(edges, nodes), edges, nodes, Set.empty[(Char, Int)]))._1.toString
  override def answer2 = (toposort(workerCount, List.empty[Char], 0, getNoIncoming(edges, nodes), edges, nodes, Set.empty[(Char, Int)]))._2.toString
}
