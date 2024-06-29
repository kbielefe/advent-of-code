package algorithms

import cats.effect.IO
import fs2.Stream
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
        return reconstructPath(current, cameFrom)

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

  sealed trait Step
  case class Visited(position: Position) extends Step
  case class Opened(positions: Set[Position]) extends Step
  case class FoundPath(path: List[Position]) extends Step
  case object Failed extends Step

  def visualize(starts: Position*): Stream[IO, Step] = {
    def helper(open: PriorityQueue[Position, Weight], opened: Set[Position], cameFrom: Map[Position, Position],
      g: Map[Position, Weight]): Stream[IO, Step] = {

      if (open.isEmpty)
        return Stream(Failed)

      val (current, openWithoutMin) = open.dequeue.get
      if (goal(current))
        return Stream(FoundPath(reconstructPath(current, cameFrom)))

      def tentativeG(neighbor: Position) = g(current) + neighborWeight(current, neighbor)

      val neighbors = getNeighbors(current) - current
      val betterNeighbors = neighbors filter {neighbor => g.get(neighbor).map(tentativeG(neighbor) < _).getOrElse(true)}

      val newCameFrom = cameFrom ++ betterNeighbors.map{(_, current)}
      val newG = g ++ betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor))}
      val updatedH = betterNeighbors.map{neighbor => (neighbor, tentativeG(neighbor) + heuristic(neighbor))}

      val newOpen = openWithoutMin.enqueue(updatedH)
      val newOpened = betterNeighbors -- opened
      val newSteps = if newOpened.isEmpty then Stream(Visited(current)) else Stream(Visited(current), Opened(newOpened))

      newSteps ++ helper(newOpen, opened ++ newOpened, newCameFrom, newG)
    }

    val open = starts.map(start => (start -> heuristic(start))).foldLeft(PriorityQueue.Empty: PriorityQueue[Position, Weight])((queue, value) => queue.enqueue(value))
    val g = starts.map(start => start -> initial).toMap
    val cameFrom = Map[Position, Position]()
    helper(open, Set.empty, cameFrom, g)
  }

  private def reconstructPath(end: Position, cameFrom: Map[Position, Position]): List[Position] =
    @tailrec
    def helper(current: Position, result: List[Position]): List[Position] =
      cameFrom.get(current) match
        case None => result
        case Some(previous) => helper(previous, previous :: result)

    helper(end, List(end))
}
