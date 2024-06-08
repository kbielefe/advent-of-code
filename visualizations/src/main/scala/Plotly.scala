package visualizations

import io.circe.*, io.circe.generic.auto.*, io.circe.syntax.*
import scala.reflect.TypeTest

trait Default
object Def extends Default

// Given is in separate trait to put it lower priority than the built-in circe
// encoders.
trait DefaultEncoder:
  given [A: Encoder](using TypeTest[A | Default, A]): Encoder[A | Default] with
    def apply(a: A | Default): Json = a match
      case a: A => a.asJson
      case _    => Json.Null

object Plotly extends DefaultEncoder:
  case class Trace[N](
    x: List[N],
    y: List[N],
    mode: String | Default = Def,
    `type`: String | Default = Def,
    text: List[String] | Default = Def
  )

  case class Axis(
    showgrid: Boolean | Default = Def
  )

  case class Layout(
    xaxis: Axis | Default = Def,
    yaxis: Axis | Default = Def,
    showlegend: Boolean | Default = Def
  )

  def apply[N: Numeric: Encoder](traces: List[Trace[N]], layout: Layout = Layout(), title: String = "Advent of Code Plot"): Unit =
    val traceJson = traces.asJson.deepDropNullValues.noSpaces
    val layoutJson = layout.asJson.deepDropNullValues.noSpaces
    val content=s"""
    <head>
      <title>$title</title>
      <style> body { margin: 0; } </style>
      <script src="https://cdn.plot.ly/plotly-2.32.0.min.js" charset="utf-8"></script>
    </head>

    <body>
      <div id="plot"></div>
      <script>
        Plotly.newPlot('plot', $traceJson, $layoutJson);
      </script>
    </body>
    """
    Browse(content, "aoc-plotly-")
