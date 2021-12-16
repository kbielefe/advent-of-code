package algorithms

import scala.collection.immutable.SortedMap

/** An immutable min priority queue that allows updating of priorities. */
class PriorityQueue[A, P] private (elementsByPriority: SortedMap[P, Set[A]], priorityByElement: Map[A, P])(using Ordering[P]):
  /** Returns if the priority queue is empty. */
  def isEmpty: Boolean = priorityByElement.isEmpty

  /** Returns the minimum element. Should check if empty first. */
  def min: A =
    elementsByPriority.head._2.head

  /** Adds or updates the given element with priority */
  def +(other: (A, P)): PriorityQueue[A, P] =
    val (a, newPriority) = other
    val newElementsByPriority = priorityByElement.get(a) match
      case None              => elementsByPriority + (newPriority -> (elementsByPriority.getOrElse(newPriority, Set.empty) + a))
      case Some(oldPriority) => elementsByPriority + (oldPriority -> (elementsByPriority(oldPriority) - a)) + (newPriority -> (elementsByPriority.getOrElse(newPriority, Set.empty) + a))
    new PriorityQueue(newElementsByPriority, priorityByElement + other)

  /** Adds or updates the given elements with their priorities */
  def ++(other: Iterable[(A, P)]): PriorityQueue[A, P] = other.foldLeft(this)(_ + _)

  /** Removes the given element, if it exists. */
  def -(element: A): PriorityQueue[A, P] =
    val exists = for
      priority <- priorityByElement.get(element)
      elements <- elementsByPriority.get(priority)
      newElements = elements - element
      newElementsByPriority = if newElements.isEmpty then elementsByPriority - priority else elementsByPriority + (priority -> newElements)
    yield new PriorityQueue(newElementsByPriority, priorityByElement - element)
    exists.getOrElse(this)

object PriorityQueue:
  def empty[A, P](using Ordering[P]): PriorityQueue[A, P] =
    new PriorityQueue(SortedMap.empty, Map.empty)

  def apply[A, P](a: (A, P)*)(using Ordering[P]): PriorityQueue[A, P] =
    empty ++ a
