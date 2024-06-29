package visualizations

import algorithms.Grid
import cats.effect.IO
import fs2.Stream
import io.circe.KeyEncoder
import io.circe.generic.auto.*
import io.circe.syntax.*
import parse.Read

given KeyEncoder[Pos] with
  def apply(pos: Pos): String =
    pos.toString

class AnimatedGrid[I: Read](
    title: String = "Advent of Code Grid Animation",
    name: String = "animated-grid",
    description: String = "An animated grid of the puzzle"
)(f: I => (Grid, Stream[IO, Frame])) extends Visualization[I](name, description):
  def show(input: I): IO[Unit] =
    val content=s"""
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
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
    val (grid, frames) = f(input)
    Browse(content, websockets = Map("frames" -> new SendOnlyWebsocket(frames)), json=Map("grid" -> grid.cells.asJson))
