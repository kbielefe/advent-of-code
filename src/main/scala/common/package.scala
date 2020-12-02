import monix.catnap.ConcurrentQueue
import monix.eval.Task
import outwatch.VDomModifier

package object common {
  type VisQueue = ConcurrentQueue[Task, VDomModifier]
}
