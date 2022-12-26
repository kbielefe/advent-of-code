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
        case Some((i, queue)) if solution(i) =>
          helper(bestI, bestN, queue)
        case Some((i, queue)) if upperBound(i) >= bestN =>
          val branches = branch(i)
          val (branchBestI, branchBestN) =
            if branches.isEmpty then
              (bestI, bestN)
            else
              branches.map(b => (b, lowerBound(b))).maxBy(_._2)
          val (newBestI, newBestN) =
            if branchBestN > bestN then
              (branchBestI, branchBestN)
            else
              (bestI, bestN)
          helper(newBestI, newBestN, queue.appendedAll(branches.filter(upperBound(_) >= newBestN)))
        case Some((i, queue)) =>
          helper(bestI, bestN, queue)

    helper(i, lowerBound(i), Queue(i))

// For a maximization problem:
// Track a global lower bound (GLB), and individual upper and lower bounds.
// If you see a LB that is greater than the GLB, update the GLB
// If you see a UB that is lower than the GLB, prune

// For a minimization problem:
// Track a global upper bound (GUB), and individual upper and lower bounds.
// If you see a UB that is less than the GUB, update the GUB
// If you see a LB that is higher than the GUB, prune
