package common
import org.scalatest._
import monix.eval.Coeval
import scala.collection.immutable.Queue

class TestGrid extends UnitSpec {
  val gridString = """
  |##########
  |#...E....#
  |#G.......#
  |##########
  """.trim.stripMargin

  abstract class Cell(char: Char)
  case class Wall() extends Cell('#')
  abstract class Creature(attackPower: Int, hitPoints: Int, gridChar: Char) extends Cell(gridChar)
  case class Elf(attackPower: Int, hitPoints: Int) extends Creature(attackPower, hitPoints, 'E')
  case class Goblin(attackPower: Int, hitPoints: Int) extends Creature(attackPower, hitPoints, 'G')

  def charToCell(char: Char): Cell = char match {
    case '#' => Wall()
    case 'E' => Elf(3, 200)
    case 'G' => Goblin(3, 200)
  }
  val grid = Grid(0, 0, '.', gridString, _ => None, charToCell _)

  def preferredParent[A](child: A, parent1: A, parent2: A): A = parent2

  "Grid" when {
    "cell retrieved" should {
      "retrieve the same cell" in {
        grid.getCell(1, 1) shouldBe None
        grid.getCell(0, 0) shouldBe Some(Wall())
        grid.getCell(4, 1) shouldBe Some(Elf(3, 200))
        grid.getCell(1, 2) shouldBe Some(Goblin(3, 200))
      }
    }

    "cell moved" should {
      "go on top of the 'to' cell" in {
        grid.move((4, 1), (4, 0)).getCell(4, 0) shouldBe Some(Elf(3, 200))
        grid.move((4, 1), (4, 0)).move((4, 0), (4, 1)).getCell(4, 0) shouldBe Some(Wall())
        grid.move((4, 1), (4, 0)).move((4, 0), (4, 1)).getCell(4, 1) shouldBe Some(Elf(3, 200))
      }
    }
  }

  "Breadth first traverse" when {
    "no neighbors" should {
      "return just the root" in {
        Grid.breadthFirstTraverse[Coeval, Int](1, _ => Queue.empty[Int], preferredParent).map{_._3}.toListL.value shouldBe List(1)
      }
    }

    "given neighbors" should {
      def neighbors(n: Int): Queue[Int] = Queue(n * 2, n * 2 + 1)
      "return those neighbors in order first" in {
        Grid.breadthFirstTraverse[Coeval, Int](1, neighbors, preferredParent).take(3).map{_._3}.toListL.value shouldBe List(1, 2, 3)
      }

      "return the next level of neighbors in order" in {
        Grid.breadthFirstTraverse[Coeval, Int](1, neighbors, preferredParent).take(7).map{_._3}.toListL.value shouldBe List(1, 2, 3, 4, 5, 6, 7)
      }

      "calculate the proper depth" in {
        Grid.breadthFirstTraverse[Coeval, Int](1, neighbors, preferredParent).take(7).map{_._2}.toListL.value shouldBe List(0, 1, 1, 2, 2, 2, 2)
      }

      "return the correct path back" in {
        val paths = Grid.breadthFirstTraverse[Coeval, Int](1, neighbors, preferredParent).take(7).map{_._1}.lastOptionL.value.get
        Grid.calculatePath(paths, 4) shouldBe List(1, 2, 4)
      }

      "return the correct path back for a looping function" in {
        def neighbors(n: Int): Queue[Int] = Queue((n + 1) % 3)
        val paths = Grid.breadthFirstTraverse[Coeval, Int](0, neighbors, preferredParent).take(10).map{_._1}.lastOptionL.value.get
        Grid.calculatePath(paths, 2) shouldBe List(0, 1, 2)
      }
    }

    "given a looping neighbors function" should {
      def neighbors(n: Int): Queue[Int] = Queue((n + 1) % 3)
      "not revisit the same neighbor in an infinite loop" in {
        Grid.breadthFirstTraverse[Coeval, Int](0, neighbors, preferredParent).take(10).map{_._3}.toListL.value shouldBe List(0, 1, 2)
      }
    }
  }
}
