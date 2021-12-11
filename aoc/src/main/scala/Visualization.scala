package visualization
import scala.compiletime.{summonFrom, summonInline}

trait Visualization[A]:
  def update(frame: A): Unit

object Visualization:
  inline def update[A](frame: A): Unit =
    summonFrom {
      case given Visualization[A] => summonInline[Visualization[A]].update(frame)
    }
