import monix.catnap.ConcurrentQueue
import monix.eval.Task
import outwatch.VDomModifier

package object common {
  type VisQueue = ConcurrentQueue[Task, VDomModifier]

  implicit class ExtendedVisQueue(val visQueue: VisQueue) extends AnyVal {
    def set(value: String): Task[Unit] =
      visQueue.offer(VDomModifier(value))
  }
}
