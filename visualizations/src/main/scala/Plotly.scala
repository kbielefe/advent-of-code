package visualizations

import io.circe.Encoder, io.circe.generic.auto.*, io.circe.syntax.*

object Plotly:
  case class Trace[N](x: List[N], y: List[N], mode: String, `type`: String)

  def apply[N: Numeric: Encoder](points: IterableOnce[(N, N)], mode: String = "markers", plotType: String = "scatter", title: Option[String] = None): Unit =
    val pointsList = points.iterator.toList
    val data = List(Trace(pointsList.map(_._1), pointsList.map(_._2), mode, plotType))
    val content=s"""
    <head>
      ${title.fold("")(t => s"<title>$t</title>")}
      <style> body { margin: 0; } </style>
      <script src="https://cdn.plot.ly/plotly-2.32.0.min.js" charset="utf-8"></script>
    </head>

    <body>
      <div id="plot"></div>
      <script>
        Plotly.newPlot('plot', ${data.asJson.noSpaces});
      </script>
    </body>
    """
    Browse(content, "aoc-scatterplot-")
