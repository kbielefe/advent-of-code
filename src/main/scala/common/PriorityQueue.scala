package algorithms
import math.Ordering.Implicits.infixOrderingOps

sealed trait PriorityQueue[+A, +P : Ordering]:
  import PriorityQueue.{Empty, Heap}

  def dequeue: Option[(A, PriorityQueue[A, P])] = this match
    case Empty                   => None
    case Heap(elem, _, subHeaps) => Some(elem, mergePairs(subHeaps))

  def enqueue[B >: A, Q >: P : Ordering](element: (B, Q)): PriorityQueue[B, Q] =
    meld(Heap(element._1, element._2, List.empty), this)

  def enqueue[B >: A, Q >: P : Ordering](elements: IterableOnce[(B, Q)]): PriorityQueue[B, Q] =
    elements.foldLeft[PriorityQueue[B, Q]](this)(_ enqueue _)

  def isEmpty: Boolean = this match
    case Empty => true
    case _     => false

  given [A, P, Q <: PriorityQueue[A, P]]: CanEqual[Q, Q] = CanEqual.derived

  private def meld[B >: A, Q >: P : Ordering](left: PriorityQueue[B, Q], right: PriorityQueue[B, Q]): PriorityQueue[B, Q] = (left, right) match
    case (Empty, Empty) => Empty
    case (Empty, right) => right
    case (left, Empty)  => left
    case (Heap(elem, priority, subHeaps), right: Heap[B, Q]) if priority < right.priority => Heap(elem, priority, right :: subHeaps)
    case (left: Heap[B, Q], Heap(elem, priority, subHeaps))                               => Heap(elem, priority, left  :: subHeaps)

  private def mergePairs(subHeaps: List[Heap[A, P]]): PriorityQueue[A, P] = subHeaps match
    case Nil            => Empty
    case x :: Nil       => x
    case x :: y :: tail => meld(meld(x, y), mergePairs(tail))

end PriorityQueue

object PriorityQueue:
  def apply[A, P : Ordering](element: (A, P)): PriorityQueue[A, P] = Empty.enqueue(element)
  case object Empty extends PriorityQueue[Nothing, Nothing]
  case class Heap[+A, +P : Ordering](elem: A, priority: P, subHeaps: List[Heap[A, P]]) extends PriorityQueue[A, P]
