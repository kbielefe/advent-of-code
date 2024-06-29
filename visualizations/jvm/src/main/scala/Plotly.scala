package visualizations

import cats.effect.IO
import io.circe.*, io.circe.generic.auto.*, io.circe.syntax.*
import scala.reflect.TypeTest
import parse.Read

trait Default
object Def extends Default

// Given is in separate trait to put it lower priority than the built-in circe
// encoders.
trait DefaultEncoder:
  given [A: Encoder](using TypeTest[A | Default, A]): Encoder[A | Default] with
    def apply(a: A | Default): Json = a match
      case a: A => a.asJson
      case _    => Json.Null

object Plotly:
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

class Plotly[N: Numeric: Encoder, I: Read](
  title: String = "Advent of Code Plot",
  name: String = "plotly",
  description: String = "Plotly plot")(f: I => List[Plotly.Trace[N]]) extends DefaultEncoder, Visualization[I](name, description):
  import Plotly.*
  def show(input: I): IO[Unit] =
    val traces = f(input)
    val content=s"""
    <head>
      <title>$title</title>
      <style> body { margin: 0; } </style>
      <script src="https://cdn.plot.ly/plotly-2.32.0.min.js" charset="utf-8"></script>
    </head>

    <body>
      <div id="plot"></div>
      <script>
        Promise.all([
          fetch('./traces'),
          fetch('./layout')
        ]).then(async([traces, layout]) =>
          Plotly.newPlot('plot', await traces.json(), await layout.json())
        )
      </script>
    </body>
    """
    val traceJson = traces.asJson.deepDropNullValues
    val layoutJson = Plotly.Layout().asJson.deepDropNullValues
    Browse(content, json=Map("traces" -> traceJson, "layout" -> layoutJson))
