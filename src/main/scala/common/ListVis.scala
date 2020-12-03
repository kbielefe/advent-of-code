package common

import monix.eval.Task
import monix.reactive.Observable
import outwatch.dsl._

object ListVis {
  def apply[A](visQueue: VisQueue, f: A => String)(observable: Observable[A]): Observable[A] =
    observable
      .scan(Seq.empty[A]){case (seq, next) => seq.appended(next)}
      .mapEval{seq =>
        val visualization = ul(seq.map(f).map(li(_)))
        visQueue.offer(visualization) *> Task(seq.last)
      }
}
