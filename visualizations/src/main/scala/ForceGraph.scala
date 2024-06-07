package visualizations

import algorithms.Graph
import io.circe.generic.auto.*, io.circe.syntax.*

object ForceGraph:
  case class Node(id: String, name: String)
  case class Link(source: String, target: String, name: String)
  case class GraphData(nodes: Set[Node], links: Set[Link])

  def forGraph[V, E](graph: Graph[V, E], title: Option[String] = None): Unit =
    val data = GraphData(
      graph.vertices.map(v => Node(v.toString, v.toString)),
      graph.edges.map(edge => Link(edge.from.toString, edge.to.toString, s"${edge.from.toString} -> ${edge.to.toString}"))
    )

    val content=s"""
    <head>
      ${title.fold("")(t => s"""<title>$t</title>""")}
      <style> body { margin: 0; } </style>
      <script src="https://cdn.jsdelivr.net/npm/force-graph/dist/force-graph.min.js" integrity="sha256-r5MkzmO7h/MwZDwEqQYsoXs74ygmI0ASGXztWV6w+Do=" crossorigin="anonymous"></script>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/force-graph/src/force-graph.min.css">
    </head>

    <body>
      <div id="graph"></div>
      <script>
        const gData = ${data.asJson.spaces2};

        const Graph = ForceGraph()
          (document.getElementById('graph'))
            .graphData(gData);
      </script>
    </body>
    """
    Browse(content, "aoc-graph-")
