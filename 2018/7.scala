import scala.io.Source
import scala.annotation.tailrec

val input = Source.fromFile("input7.txt").getLines.toList

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
