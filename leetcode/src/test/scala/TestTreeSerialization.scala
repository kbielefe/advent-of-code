import cats.Eval, cats.data.Cont, cats.syntax.all.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.Queue

case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

object Tree:
  // In Cont[A, B], B is the argument to the continuation and what gets
  // "extracted" in a flatMap. A is the result of the continuation.

  def fromLevelOrder[A](levelOrder: List[Option[A]]): Option[Tree[A]] =
    type LevelOrder = List[Option[A]]
    type Node = Option[Tree[A]]

    def findChild(levelOrder: LevelOrder): Cont[Node, Node] = levelOrder.headOption.flatten match
      case None    => Cont.pure(None)
      case Some(a) => Cont.pure(Some(Tree(a, None, None)))

    def findTree(levelOrder: LevelOrder): Cont[Node, Node] = for
      left  <- findChild(levelOrder.tail)
      right <- findChild(levelOrder.tail.tail)
    yield Some(Tree(levelOrder.head.get, left, right))

    findTree(levelOrder).eval.value

class TestTreeSerialization extends AnyWordSpec with Matchers {
  "Delimited continuations" should {
    "have an example" ignore {
      val result: Cont[Unit, String] = Cont.reset[String, Unit]:
        for
          _ <- Cont.defer(println("Before shift"))
          x <- Cont.shift[String, String]:k =>
            for
              _ <- Cont.defer(println("Before continue"))
              y <- Cont.liftF(k("x"))
              _ <- Cont.defer(println(s"y: $y"))
            yield "z"
          _ <- Cont.defer(println(s"x: $x"))
        yield "y"
      result.run(z => Eval.later(println(s"z: $z"))).value
    }
  }

  "Tree Serialization" when {
    "given an empty list" should {
      "return an empty tree" in {
        Tree.fromLevelOrder[Int](List.empty) shouldBe None
      }
    }

    "given [1,2,3]" should {
      "return a tree with root 1 and children 2 and 3" in {
        Tree.fromLevelOrder(List(Some(1), Some(2), Some(3))) shouldBe Some(Tree(1, Some(Tree(2, None, None)), Some(Tree(3, None, None))))
      }
    }
  }
}
