package common

import monix.catnap.ConcurrentQueue
import monix.eval.Task
import outwatch.VDomModifier

object StringVis {
  def set(visQueue: VisQueue, value: String): Task[Unit] =
    visQueue.offer(VDomModifier(value))
}
