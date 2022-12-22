package algorithms
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import math.Ordering.Implicits.given

// TODO: add a min once debugged
// TODO: allow any kind of queue: FIFO, Stack, or priority queue.
/** Find a solution state `I` that maximizes the objective function */
def branchAndBoundMax[I, N : Ordering](
  objective:  I => N,        // The function being maximized
  branch:     I => Set[I],   // Generate a set of (usually disjoint) candidate sub-solutions
  lowerBound: I => N,        // Find the lower bound on the value of any candidate solution in I (lowerBound(I) <= objective(x) for all x candidate solutions in I.
  upperBound: I => N,        // Find the upper bound on the value of any candidate solution in I (upperBound(I) >= objective(x) for all x candidate solutions in I.
  solution:   I => Boolean   // Returns if I represents a single candidate solution (leaf).
  )(i: I): (I, N) =
    @tailrec
    def helper(bestI: I, bestN: N, queue: Queue[I]): (I, N) =
      queue.dequeueOption match
        case None =>
          (bestI, bestN)
        case Some((i, queue)) if solution(i) && objective(i) > bestN =>
          helper(i, objective(i), queue)
        case Some((i, queue)) =>
          helper(bestI, bestN, queue.appendedAll(branch(i).filter(upperBound(_) >= bestN)))

    helper(i, lowerBound(i), Queue(i))
