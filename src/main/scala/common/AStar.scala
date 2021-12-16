package algorithms
import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

class AStar[Position, Weight : Numeric](
    heuristic: (Position, Position) => Weight,
    edgeWeight: (Position, Position) => Weight,
    startWeight: Weight,
    getNeighbors: Position => Set[Position])(using CanEqual[Position, Position]) {

  def getPath(start: Position, goal: Position) : List[Position] = {
    @tailrec
    def helper(open: PriorityQueue[Position, Weight], cameFrom: Map[Position, Position],
      g: Map[Position, Weight]): List[Position] = {

      if (open.isEmpty)
        return List.empty[Position]

      val current = open.min
      if (current == goal)
        return reconstructPath(start, goal, cameFrom)

      def tentativeG(neighbor: Position) = g(current) + edgeWeight(current, neighbor)

      val neighbors = getNeighbors(current)
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).map(tentativeG(neighbor) < _).getOrElse(true)}

      val newCameFrom = cameFrom ++ betterNeighbors.map{(_, current)}
      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor, goal))}

      val newOpen = open ++ updatedH - current

      helper(newOpen, newCameFrom, newG)
    }

    val open = PriorityQueue(start -> heuristic(start, goal))
    val cameFrom = Map[Position, Position]()
    val g = Map[Position, Weight](start -> startWeight)
    helper(open, cameFrom, g)
  }

  private def reconstructPath(start: Position, goal: Position, cameFrom: Map[Position, Position]): List[Position] = {
    @tailrec
    def helper(current: Position, result: List[Position]): List[Position] = {
      if (current == start)
        return start :: result
      helper(cameFrom(current), current :: result)
    }

    helper(goal, List.empty[Position])
  }
}
