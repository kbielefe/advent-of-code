package visualizations

import algorithms.Grid
import cats.effect.IO
import fs2.Stream
import io.circe.KeyEncoder
import io.circe.generic.auto.*
import io.circe.syntax.*

given KeyEncoder[Pos] with
  def apply(pos: Pos): String =
    pos.toString

object AnimatedGrid:
  def apply(grid: Grid, frames: Stream[IO, Frame], title: String = "Advent of Code Grid Animation"): Unit =
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
    Browse(content, websockets = Map("frames" -> new SendOnlyWebsocket(frames)), json=Map("grid" -> grid.cells.asJson))
