import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

object Tree:
  def fromLevelOrder[A](levelOrder: IndexedSeq[Option[A]]): Option[Tree[A]] =
    def postOrder(col: Int, levelOrder: IndexedSeq[Option[A]], nodeCount: Int): Option[Tree[A]] =
      if levelOrder.isEmpty then
        None
      else
        val newCol = (col - levelOrder.take(col).count(!_.isDefined)) * 2
        val newLevelOrder = levelOrder.drop(nodeCount)
        val newNodeCount = levelOrder.take(nodeCount).count(_.isDefined) * 2
        val left  = postOrder(newCol,     newLevelOrder, newNodeCount)
        val right = postOrder(newCol + 1, newLevelOrder, newNodeCount)
        val value = levelOrder.lift(col).flatten
        value.map(Tree(_, left, right))
    end postOrder

    postOrder(0, levelOrder, 1)

class TestTreeSerialization extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {
  val trees = Table(
    "level order" -> "tree",
    Vector.empty    -> None,
    Vector(None)    -> None,
    Vector(Some(1)) -> Some(Tree(1, None, None)),
    Vector(Some(1), Some(2)) -> Some(Tree(1, Some(Tree(2, None, None)), None)),
    Vector(Some(1), None, Some(2), Some(3)) -> Some(Tree(1, None, Some(Tree(2,Some(Tree(3, None, None)),None)))),
    Vector(Some(1), None, Some(2), None, Some(3), None, Some(4)) -> Some(Tree(1, None, Some(Tree(2,None, Some(Tree(3, None, Some(Tree(4, None, None)))))))),
    Vector(Some(5),Some(4),Some(7),Some(3),None,Some(2),None,Some(-1),None,Some(9)) -> Some(Tree(5, Some(Tree(4, Some(Tree(3, Some(Tree(-1, None, None)), None)), None)), Some(Tree(7, Some(Tree(2, Some(Tree(9, None, None)), None)), None)))),
  )

  "Tree.fromLevelOrder" should {
    "generate trees from a level order properly" in {
      forAll (trees): (levelOrder, tree) =>
        Tree.fromLevelOrder(levelOrder) shouldBe tree
    }
  }
}
