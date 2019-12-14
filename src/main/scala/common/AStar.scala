package common

class AStar[Position](
    heuristic: (Position, Position) => Double,
    edgeWeight: (Position, Position) => Double,
    getNeighbors: Position => Set[Position]) {
  import scala.annotation.tailrec

  def getPath(start: Position, goal: Position) : List[Position] = {
    @tailrec
    def helper(closed: Set[Position], open: Set[Position], cameFrom: Map[Position, Position],
      g: Map[Position, Double], f: Map[Position, Double]): List[Position] = {

      if (open.isEmpty)
        return List[Position]()

      val current = open minBy {f(_)}
      if (current == goal)
        return reconstructPath(start, goal, cameFrom)

      def tentativeG(neighbor: Position) = g(current) + edgeWeight(current, neighbor)

      val neighbors = getNeighbors(current) -- closed
      val notOpenNeighbors = neighbors -- open
      val betterNeighbors = (neighbors & open) filter {neighbor => tentativeG(neighbor) < g(neighbor)}
      val newNeighbors = notOpenNeighbors ++ betterNeighbors

      val newCameFrom = cameFrom ++ (newNeighbors map {(_, current)})
      val newG = g ++ (newNeighbors map {neighbor => (neighbor, tentativeG(neighbor))})
      val newF = f ++ (newNeighbors map {neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor, goal))})

      val newClosed = closed + current
      val newOpen = open ++ newNeighbors - current

      helper(newClosed, newOpen, newCameFrom, newG, newF)
    }

    val closed = Set[Position]()
    val open = Set(start)
    val cameFrom = Map[Position, Position]()
    val g = Map[Position, Double](start -> 0.0)
    val f = Map[Position, Double](start -> heuristic(start, goal))
    helper(closed, open, cameFrom, g, f)
  }

  private def reconstructPath(start: Position, goal: Position, cameFrom: Map[Position, Position]): List[Position] = {
    @tailrec
    def helper(current: Position, result: List[Position]): List[Position] = {
      if (current == start)
        return start :: result
      helper(cameFrom(current), current :: result)
    }

    helper(goal, List[Position]())
  }
}
