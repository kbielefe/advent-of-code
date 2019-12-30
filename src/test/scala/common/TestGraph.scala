package common.graph
import org.scalatest._
import monix.execution.Scheduler.Implicits.global

class TestGraph extends common.UnitSpec {
  def comesBefore(a: Int, b: Int, list: List[Int]): Boolean = {
    list.filter(x => x == a || x == b) == List(a, b)
  }

  "depthFirst" when {
    "empty" should {
      "return empty" in {
        val g = new Graph[Int, Int](Set.empty, _ => Set.empty)
        val obs = g.depthFirst(Node(0, 0), _ => false)
        obs.isEmptyL.runSyncUnsafe() shouldBe true
      }
    }

    "contains a single element" should {
      "return that element" in {
        val g = new Graph[Int, Int](Set(Node(1, 1)), _ => Set.empty)
        val obs = g.depthFirst(Node(1, 1), _ => false)
        obs.toListL.runSyncUnsafe() shouldBe List(Node(1, 1))
      }

      "return empty when asked for root not in the graph" in {
        val g = new Graph[Int, Int](Set(Node(1, 1)), _ => Set.empty)
        val obs = g.depthFirst(Node(0, 0), _ => false)
        obs.isEmptyL.runSyncUnsafe() shouldBe true
      }
    }

    "contains 3 elements in a tree" should {
      val g = new Graph[Int, Int](Set(Node(1, 1), Node(2, 2), Node(3, 3)), n => Set(n * 2, n * 2 + 1))
      "return 2 and 3 after 1" in {
        val obs = g.depthFirst(Node(1, 1), _ => true)
        val result = obs.toListL.runSyncUnsafe().map(_.data)
        assert(result == List(1, 2, 3) || result == List(1, 3, 2))
      }

      "return only node 2 when that is the root" in {
        val obs = g.depthFirst(Node(2, 2), _ => true)
        val result = obs.toListL.runSyncUnsafe().map(_.data)
        result shouldBe List(2)
      }

      "return only node 3 when that is the root" in {
        val obs = g.depthFirst(Node(3, 3), _ => true)
        val result = obs.toListL.runSyncUnsafe().map(_.data)
        result shouldBe List(3)
      }
    }

    "contains a 3-level DAG" should {
      val nodes = (1 to 10).map(node => Node(node, node)).toSet
      val g = new Graph[Int, Int](nodes, n => Set(n * 2, n * 2 + 1, n * 2 + 2))
      "contain each element exactly once" in {
        val obs = g.depthFirst(Node(1, 1), _ => true)
        obs.toListL.runSyncUnsafe().map(_.data).sorted shouldBe (1 to 10).toList
      }

      "return in depth-first order" in {
        val obs = g.depthFirst(Node(1, 1), _ => true)
        val result = obs.toListL.runSyncUnsafe().map(_.data)
        println(result)
        if (comesBefore(2, 3, result)) {
          assert(comesBefore(4, 3, result))
        } else {
          assert(comesBefore(6, 2, result))
        }
      }
    }
  }
}
