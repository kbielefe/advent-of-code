package visualizations

import cats.effect.IO
import parse.Read

trait NormalizedVisualization:
  val name: String
  val description: String
  def show(input: String): IO[Unit]

trait Visualization[I: Read](val name: String, val description: String) extends NormalizedVisualization:
  def show(input: I): IO[Unit]
  override def show(input: String): IO[Unit] =
    show(summon[Read[I]].read(input))
