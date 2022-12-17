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

  // TODO: Allow taking a list of start positions that all get added to the open queue.
  // TODO: Add visualization
  // To find max reward instead:
  //   - make the neighbor weights negative
  //   - make sure the heuristic is more negative (larger absolute value) than the real reward. (maybe not?)
  //   - make sure it can't loop indefinitely (or at all?)
  //   - negate the result
  def getMinCost(start: Position): Option[Weight] = {
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

    val open = PriorityQueue(start -> heuristic(start))
    val g = Map[Position, Weight](start -> initial)
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
