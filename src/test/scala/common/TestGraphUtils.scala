package common
import org.scalatest._
import scala.io.Source
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

class TestGraphUtils extends UnitSpec {
  val g = Graph(0~>1, 1~>2, 2~>0)
  "move" when {
    "moving 0 times" should {
      "return the same node" in {
        GraphUtils.move(g, 0, 0) shouldBe 0
      }
    }

    "moving 1 forward" should {
      "return the node's successor" in {
        GraphUtils.move(g, 0, 1) shouldBe 1
      }
    }

    "moving 2 forward" should {
      "return the node's successor of successor" in {
        GraphUtils.move(g, 0, 2) shouldBe 2
      }
    }

    "moving 3 forward" should {
      "wrap around" in {
        GraphUtils.move(g, 0, 3) shouldBe 0
      }
    }

    "moving 1 backward" should {
      "wrap around backward" in {
        GraphUtils.move(g, 0, -1) shouldBe 2
      }
    }

    "moving 2 backward" should {
      "return the second predecessor" in {
        GraphUtils.move(g, 0, -2) shouldBe 1
      }
    }
  }

  "insertAfterCircular" when {
    "inserting after beginning" should {
      "insert" in {
        GraphUtils.insertAfterCircular(g, 0, 4) shouldBe Graph(0~>4, 4~>1, 1~>2, 2~>0)
      }
    }

    "inserting after last" should {
      "insert" in {
        GraphUtils.insertAfterCircular(g, 2, 4) shouldBe Graph(0~>1, 1~>2, 2~>4, 4~>0)
      }
    }
  }

  "deleteCircular" when {
    "deleting first" should {
      "delete" in {
        GraphUtils.deleteCircular(g, 0) shouldBe Graph(1~>2, 2~>1)
      }
    }

    "deleting last" should {
      "delete" in {
        GraphUtils.deleteCircular(g, 2) shouldBe Graph(0~>1, 1~>0)
      }
    }
  }
}
