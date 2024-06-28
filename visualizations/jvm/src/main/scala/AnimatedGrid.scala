package visualizations

import algorithms.Grid, Grid.Pos
import cats.effect.IO
import fs2.Stream

object AnimatedGrid:
  sealed trait Change
  case class ChangeChar(char: Char)         extends Change
  case class ChangeColor(color: String)     extends Change
  case class ChangeTooltip(tooltip: String) extends Change

  case class Frame(changes: Map[Pos, Seq[Change]], description: Option[String] = None)

  def apply(initial: Grid, frames: Stream[IO, Frame], title: String = "Advent of Code Grid Animation"): Unit =
    val content=s"""
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="utf-8">
        <title>$title</title>
        <style> body { margin: 0; } </style>
      </head>

      <body>
        <div id="grid"></div>
        <script type="module">
          import {AnimatedGrid} from './animated-grid.js';
          AnimatedGrid.animate();
        </script>
      </body>
      </html>
    """
    Browse(content) // TODO: create a websocket for the frames
