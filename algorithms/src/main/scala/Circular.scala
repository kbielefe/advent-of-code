package algorithms

import scala.collection.mutable.Map

/** A mutable circular linked list with fast lookup by value, and fast inserts
 *  and deletions. */
class Circular[A] private (nodesByValue: Map[A, Circular.Node[A]]):
  def removeNAfter(n: Int, after: A): List[A] =
    val prev = nodesByValue(after)
    val removedNodes = Iterator.iterate(prev)(_.next).drop(1).take(n).toList
    prev.next = removedNodes.last.next
    val result = removedNodes.map(_.value)
    nodesByValue --= result
    result

  def insertAfter(list: List[A], after: A): Unit =
    val prev = nodesByValue(after)
    val next = prev.next
    val lastNode = list.foldLeft(prev){(prev, a) =>
      val node = new Circular.Node[A](a, null)
      nodesByValue += (a -> node)
      prev.next = node
      node
    }
    lastNode.next = next

  def next(after: A): A =
    nodesByValue(after).next.value

object Circular:
  def apply[A](elements: IterableOnce[A]): Circular[A] =
    val i = elements.iterator
    val head = i.next()
    val headNode = new Node[A](head, null)
    val map = Map(head -> headNode)
    val lastNode = i.foldLeft(headNode){(prev, a) =>
      val node = new Node[A](a, null)
      map += (a -> node)
      prev.next = node
      node
    }
    lastNode.next = headNode
    new Circular[A](map)

  private class Node[A](val value: A, var next: Node[A])
