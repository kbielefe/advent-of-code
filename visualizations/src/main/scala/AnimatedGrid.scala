package visualizations

import algorithms.Grid, Grid.Pos
import io.circe.*, io.circe.generic.auto.*, io.circe.syntax.*

object AnimatedGrid:
  case class Step(chars: Map[Pos, Char], colors: Map[Pos, String])

  def apply(initial: Grid, steps: List[Step], title: String = "Advent of Code Grid Animation"): Unit =
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
          const initial = ${initial.asJson.noSpaces};
          const steps = ${steps.asJson.noSpaces};
          AnimatedGrid.animate(initial, steps);
        </script>
      </body>
      </html>
    """
    Browse(content)
