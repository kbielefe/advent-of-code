package algorithms
import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

class AStar[Position, Weight : Numeric](
    goal: Position => Boolean,
   // The heuristic must be <= the actual cost, but is more efficient when
   // closer to the actual cost. _ => 0 is equivalent to Dijkstra's algorithm.
    heuristic: Position => Weight,
    neighborWeight: (Position, Position) => Weight, // current -> neighbor -> weight
    initial: Weight, // initial weight of the start state, usually zero
    getNeighbors: Position => Set[Position]) {

  def getPath(start: Position)(using CanEqual[Position, Position]) : List[Position] = {
    @tailrec
    def helper(open: PriorityQueue[Position, Weight], cameFrom: Map[Position, Position],
      g: Map[Position, Weight]): List[Position] = {

      if (open.isEmpty)
        return List.empty[Position]

      val (current, openWithoutMin) = open.dequeue.get
      if (goal(current))
        return reconstructPath(start, current, cameFrom)

      def tentativeG(neighbor: Position) = g(current) + neighborWeight(current, neighbor)

      val neighbors = getNeighbors(current) - current
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).map(tentativeG(neighbor) < _).getOrElse(true)}

      val newCameFrom = cameFrom ++ betterNeighbors.map{(_, current)}
      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor))}

      val newOpen = openWithoutMin.enqueue(updatedH)

      helper(newOpen, newCameFrom, newG)
    }

    val open = PriorityQueue(start -> heuristic(start))
    val cameFrom = Map[Position, Position]()
    val g = Map[Position, Weight](start -> initial)
    helper(open, cameFrom, g)
  }

  // TODO: Add visualization
  def getMinCost(starts: Position*): Option[Weight] = {
    @tailrec
    def helper(open: PriorityQueue[Position, Weight], g: Map[Position, Weight]): Option[Weight] = {

      if (open.isEmpty)
        return None

      val (current, openWithoutMin) = open.dequeue.get
      if (goal(current))
        return Some(g(current))

      def tentativeG(neighbor: Position) = g(current) + neighborWeight(current, neighbor)

      val neighbors = getNeighbors(current) - current
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).map(tentativeG(neighbor) < _).getOrElse(true)}

      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor))}

      val newOpen = openWithoutMin.enqueue(updatedH)

      helper(newOpen, newG)
    }

    val open = starts.map(start => (start -> heuristic(start))).foldLeft(PriorityQueue.Empty: PriorityQueue[Position, Weight])((queue, value) => queue.enqueue(value))
    val g = starts.map(start => start -> initial).toMap
    helper(open, g)
  }

  private def reconstructPath(start: Position, end: Position, cameFrom: Map[Position, Position])(using CanEqual[Position, Position]): List[Position] = {
    @tailrec
    def helper(current: Position, result: List[Position]): List[Position] = {
      if (current == start)
        return start :: result
      helper(cameFrom(current), current :: result)
    }

    helper(end, List.empty[Position])
  }
}
