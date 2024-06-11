package visualizations

import algorithms.Graph
import io.circe.generic.auto.*, io.circe.syntax.*

object ForceGraph:
  case class Node(id: String, name: String)
  case class Link(source: String, target: String, name: String)
  case class GraphData(nodes: Set[Node], links: Set[Link])

  sealed trait Config(val command: String)
  case class LinkDirectionalParticles(num: Int) extends Config(s".linkDirectionalParticles($num)")

  def forGraph[V, E](graph: Graph[V, E], title: Option[String] = None, config: List[Config] = List.empty): Unit =
    val data = GraphData(
      graph.vertices.map(v => Node(v.toString, v.toString)),
      graph.edges.map(edge => Link(edge.from.toString, edge.to.toString, s"${edge.from.toString} -[${edge.props}]-> ${edge.to.toString}"))
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
        const Graph = ForceGraph()
          (document.getElementById('graph'))
            .graphData(${data.asJson.spaces2})${config.map(_.command).mkString};
      </script>
    </body>
    """
    Browse(content, "aoc-graph-")
