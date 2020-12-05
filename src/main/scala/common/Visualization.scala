package common

import colibri.ext.monix._
import monix.reactive.Observable
import outwatch._
import outwatch.dsl._
import outwatch.reactive.handlers.monix._

object Visualization {
  sealed trait Event
  case object Clear                                       extends Event
  case class  Error(e: Throwable)                         extends Event
  case class  ChangePuzzle(vis: Observable[VDomModifier]) extends Event

  def apply(events: Handler[Event]): Observable[VDomModifier] =
    events.collect{
      case Clear           => VDomModifier.empty
      case Error(e)        => exceptionString(e)
      case ChangePuzzle(_) => VDomModifier.empty
    }

  private def exceptionString(e: Throwable): VNode = {
    val os = new java.io.ByteArrayOutputStream()
    val ps = new java.io.PrintStream(os)
    e.printStackTrace(ps)
    ps.flush()
    val result = os.toString
    ps.close()
    os.close()
    pre(result)
  }
}
