package algorithms

import scala.collection.immutable.Queue

/** A Queue that only allows a given value to be queued once */
class QueueOnce[+A](queue: Queue[A], enqueued: Set[A]):
  def dequeueOption: Option[(A, QueueOnce[A])] =
    queue.dequeueOption.map((a, queue) => (a, new QueueOnce[A](queue, enqueued)))

  def enqueue[B >: A](a: B): QueueOnce[B] =
    if enqueued.asInstanceOf[Set[B]].contains(a) then
      this
    else
      new QueueOnce[B](queue.enqueue(a), enqueued.asInstanceOf[Set[B]] + a)

object QueueOnce:
  def apply[A](as: A*): QueueOnce[A] =
    new QueueOnce[A](Queue(as*), Set.empty)
