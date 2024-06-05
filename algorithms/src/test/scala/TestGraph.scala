import org.scalatest.enablers.Emptiness
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import algorithms.{Graph, Edge}

class TestGraph extends AnyWordSpec with Matchers {
  given [V, E]: Emptiness[Graph[V, E]] with
    def isEmpty(g: Graph[V, E]): Boolean =
      g.vertices.isEmpty

  "A Graph" when {
    "empty" should {
      val graph = Graph.fromEdges[Int, Int](Set.empty)

      "have no vertices with no incoming edges" in {
        graph.noIncoming shouldBe empty
      }

      "have an empty toposort" in {
        graph.toposort shouldBe empty
      }

      "allow adding a vertex" in {
        (graph + 1).vertices.size shouldBe 1
        (graph + 1).edges.size shouldBe 0
      }

      "still be empty when deleting a non-existent vertex" in {
        (graph - 1) shouldBe empty
      }
    }

    "Containing a single vertex" should {
      val graph = Graph.empty[Int, Int] + 1

      "have no edges" in {
        graph.edges shouldBe empty
      }

      "have no incoming edges" in {
        graph.incomingEdges(1) shouldBe empty
      }

      "have no outgoing edges" in {
        graph.outgoingEdges(1) shouldBe empty
      }

      "have that vertex be the only one with no incoming edges" in {
        graph.noIncoming shouldBe Set(1)
      }

      "have that vertex be the only one in the toposort" in {
        graph.toposort.toList shouldBe List(1)
      }

      "return the same graph reachable from that vertex" in {
        graph.reachableFrom(1) shouldBe graph
      }

      "be empty when removing that vertex" in {
        (graph - 1) shouldBe empty
      }

      "allow adding a second vertex" in {
        (graph + 2).vertices.size shouldBe 2
      }

      "deduplicate a newly added vertex" in {
        (graph + 1) shouldBe graph
      }
    }

    "Containing two vertices connected by an edge" should {
      val graph = Graph.fromEdges(List(Edge(1, 2, ())))

      "contain both vertices" in {
        graph.vertices.size shouldBe 2
      }

      "have empty incoming edges for the first node" in {
        graph.incomingEdges.get(1) shouldBe empty
      }

      "have the edge as an incoming edge for the second node" in {
        graph.incomingEdges(2) shouldBe Set(Edge(1, 2, ()))
      }

      "have the edge as an outgoing edge for the first node" in {
        graph.outgoingEdges(1) shouldBe Set(Edge(1, 2, ()))
      }

      "have empty outgoing edges for the second node" in {
        graph.outgoingEdges.get(2) shouldBe empty
      }

      "indicate the first node as having no incoming edges" in {
        graph.noIncoming shouldBe Set(1)
      }

      "remove the edge when it removes the first node" in {
        (graph - 1).edges shouldBe empty
      }

      "remove the edge when it removes the second node" in {
        (graph - 2).edges shouldBe empty
      }

      "have the correct toposort" in {
        graph.toposort.toList shouldBe List(1, 2)
      }
    }
  }
}
