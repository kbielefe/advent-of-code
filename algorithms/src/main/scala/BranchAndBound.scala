package algorithms
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import math.Ordering.Implicits.given

case class Variable[N : Ordering](name: String, value: Option[N], lowerBound: Option[N], upperBound: Option[N]):
  override def toString: String =
    s"$name: ${lowerBound.map(_.toString + " <= ").getOrElse("")}${value.map(_.toString).getOrElse("unassigned")}${upperBound.map(" <= " + _.toString).getOrElse("")}"

// Problem-specific details for the implementation.
trait BranchAndBound[I, N : Ordering]:
  // The function being maximized
  def objective(state: I): N

  // Generate a set of (usually disjoint) candidate sub-solutions. Will not be
  // called for leaves.
  def branch(state: I): Set[I]

  // Find the lower bound on the value of any candidate solution
  // (lowerBound(I) <= objective(x) for all x candidate solutions
  def lowerBound(state: I): N

  // Find the upper bound on the value of any candidate solution
  // (upperBound(I) >= objective(x) for all x candidate solutions
  def upperBound(state: I): N

  // Do the variables represent a single candidate solution (leaf)?
  def solution(state: I): Boolean

trait QueueLike[Q[_]]:
  extension [A] (q: Q[A])
    def dequeueOption: Option[(A, Q[A])]
    def appendedAll(items: IterableOnce[A]): Q[A]

given QueueLike[Queue] with
  extension [A] (q: Queue[A])
    def dequeueOption: Option[(A, Queue[A])] = q.dequeueOption
    def appendedAll(items: IterableOnce[A]): Queue[A] = q.appendedAll(items)

given QueueLike[List] with
  extension [A] (q: List[A])
    def dequeueOption: Option[(A, List[A])] =
      q.headOption.map(a => (a, q.tail))
    def appendedAll(items: IterableOnce[A]): List[A] =
      q.prependedAll(items)

// TODO: add a min once debugged
// TODO: add memoization
// TODO: add cameFrom
// TODO: add parallelization

/** Find a solution state `I` that maximizes the objective function using a
 *  FIFO queue (BFS). Will return initial state if nothing better is found.
 */
def branchAndBoundMaxFifo[I, N : Ordering](i: I)(using BranchAndBound[I, N]): (I, N) =
  branchAndBoundGeneric(Queue(i), i)

/** Find a solution state `I` that maximizes the objective function using a
 *  LIFO queue (DFS). Will return initial state if nothing better is found.
 */
def branchAndBoundMaxLifo[I, N : Ordering](i: I)(using BranchAndBound[I, N]): (I, N) =
  branchAndBoundGeneric(List(i), i)

/** Find a solution state `I` that maximizes the objective function using a
 *  priority queue. States that come earliest according to the `Ordering[I]`
 *  will be visited first. Will return initial state if nothing better is
 *  found.
 */
def branchAndBoundMaxPriority[I, N : Ordering](i: I)(using BranchAndBound[I, N], Ordering[I]): (I, N) =
  type Q[A] = PriorityQueue[A, I]
  given QueueLike[Q] with
    extension [A] (q: Q[A])
      def dequeueOption: Option[(A, Q[A])] =
        q.dequeue
      def appendedAll(items: IterableOnce[A]): Q[A] =
        q.enqueue(items.iterator.map(item => (item, item.asInstanceOf[I])))
  branchAndBoundGeneric[I, N, Q](PriorityQueue[I, I](i -> i), i)

private[algorithms] def branchAndBoundGeneric[I, N : Ordering, Q[_] : QueueLike](
  queue: Q[I], // The open queue containing the initial state.
  i: I         // The initial state
  )(using impl: BranchAndBound[I, N]): (I, N) =
    @tailrec
    def helper(bestI: I, bestN: N, queue: Q[I]): (I, N) =
      queue.dequeueOption match
        case None =>
          (bestI, bestN)
        case Some((i, queue)) if impl.solution(i) && impl.objective(i) > bestN =>
          helper(i, impl.objective(i), queue)
        case Some((i, queue)) if impl.solution(i) =>
          helper(bestI, bestN, queue)
        case Some((i, queue)) if impl.upperBound(i) > bestN =>
          val branches = impl.branch(i)
          val (branchBestI, branchBestN) =
            if branches.isEmpty then
              (bestI, bestN)
            else
              branches.map(b => (b, impl.lowerBound(b))).maxBy(_._2)
          val (newBestI, newBestN) =
            if branchBestN > bestN then
              (branchBestI, branchBestN)
            else
              (bestI, bestN)
          helper(newBestI, newBestN, queue.appendedAll(branches.filter(impl.upperBound(_) > newBestN)))
        case Some((i, queue)) =>
          helper(bestI, bestN, queue)

    helper(i, impl.lowerBound(i), queue)

// For a maximization problem:
// Track a global lower bound (GLB), and individual upper and lower bounds.
// If you see a LB that is greater than the GLB, update the GLB
// If you see a UB that is lower than the GLB, prune

// For a minimization problem:
// Track a global upper bound (GUB), and individual upper and lower bounds.
// If you see a UB that is less than the GUB, update the GUB
// If you see a LB that is higher than the GUB, prune
