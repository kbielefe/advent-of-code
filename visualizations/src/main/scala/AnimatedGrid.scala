package visualizations

import algorithms.Grid, Grid.Pos
import io.circe.*, io.circe.generic.auto.*, io.circe.syntax.*

object AnimatedGrid:
  case class Step(chars: Map[Pos, Char], colors: Map[Pos, String])

  def apply(initial: Grid, steps: List[Step], title: String = "Advent of Code Grid Animation"): Unit =
    val content=s"""
    <head>
      <title>$title</title>
      <style> body { margin: 0; } </style>
    </head>

    <body>
      <div id="grid"></div>
      <script>
        const initial = ${initial.asJson.noSpaces};
        const steps = ${steps.asJson.noSpaces};
      </script>
    </body>
    """
    Browse(content)
