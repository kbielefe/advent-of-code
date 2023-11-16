package algorithms
import scala.annotation.tailrec
import scala.collection.immutable.Queue

def floodFill[Position](start: Position, getNeighbors: Position => Set[Position]): Set[Position] =
  @tailrec
  def helper(visited: Set[Position], open: Queue[Position]): Set[Position] =
    open.dequeueOption match
      case Some((current, open)) =>
        val neighbors = getNeighbors(current) -- visited - current
        val newVisited = visited ++ neighbors
        val newOpen = open.enqueueAll(neighbors)
        helper(newVisited, newOpen)
      case None =>
        visited
  helper(Set(start), Queue(start))
