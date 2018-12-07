import scala.io.Source
import scala.annotation.tailrec

val example = false
val input = if (example)
    Source.fromFile("input7example.txt").getLines.toList
  else
    Source.fromFile("input7.txt").getLines.toList

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

@tailrec
def toposort(result: List[Char], noIncoming: Set[Char], edges: Map[Char, Set[Char]], nodes: Set[Char]): String = {
  if (noIncoming.isEmpty)
    result.reverse.mkString("")
  else {
    val next = noIncoming.toList.sorted.head
    val newResult = next :: result
    val newNodes = nodes - next
    val newEdges = edges - next
    val newNoIncoming = getNoIncoming(newEdges, newNodes)
    toposort(newResult, newNoIncoming, newEdges, newNodes)
  }
}

val answer1 = toposort(List.empty[Char], getNoIncoming(edges, nodes), edges, nodes)
println(answer1)

val offset = if (example) 1 else 61
def duration(task: Char): Int = task - 'A' + offset
val workerCount = if (example) 2 else 5 

@tailrec
def parallelToposort(result: Int, noIncoming: Set[Char], edges: Map[Char, Set[Char]], nodes: Set[Char], wip: Set[(Char, Int)]): Int = {
  if (noIncoming.isEmpty)
    result - 1
  else {
    val newResult = result + 1
    val (carryoverWip, completed) = wip map {case (task, remaining) => (task, remaining - 1)} partition {_._2 > 0}
    val completedTasks = completed map {_._1}
    val carryoverTasks = carryoverWip map {_._1}
    val newNodes = nodes -- completedTasks
    val newEdges = edges -- completedTasks
    val newNoIncoming = getNoIncoming(newEdges, newNodes)

    val availableWorkers = workerCount - carryoverWip.size
    val newTasks = (newNoIncoming -- carryoverTasks).toList.sorted.take(availableWorkers).toSet

    val newWip = carryoverWip ++ (newTasks map {task => (task, duration(task))})
    parallelToposort(newResult, newNoIncoming, newEdges, newNodes, newWip)
  }
}

val answer2 = parallelToposort(0, getNoIncoming(edges, nodes), edges, nodes, Set.empty[(Char, Int)])
println(answer2)
