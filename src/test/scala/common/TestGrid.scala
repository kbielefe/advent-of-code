package common
import org.scalatest._

class TestGrid extends UnitSpec {
  val gridString = """
  |##########
  |#...E....#
  |#G.......#
  |##########
  """.trim.stripMargin

  abstract class Cell(gridChar: Char) extends Grid.Cell {
    override def char = gridChar
  }
  case class Wall() extends Cell('#')
  abstract class Creature(attackPower: Int, hitPoints: Int, gridChar: Char) extends Cell(gridChar)
  case class Elf(attackPower: Int, hitPoints: Int) extends Creature(attackPower, hitPoints, 'E')
  case class Goblin(attackPower: Int, hitPoints: Int) extends Creature(attackPower, hitPoints, 'G')

  def charToCell(char: Char): Cell = char match {
    case '#' => Wall()
    case 'E' => Elf(3, 200)
    case 'G' => Goblin(3, 200)
  }
  val grid = Grid(0, 0, '.', gridString, x => if ("EG" contains x) Some('.') else None, charToCell _)

  "Grid" when {
    "cell retrieved" should {
      "retrieve the same cell" in {
        grid.getCell(1, 1) shouldBe None
        grid.getCell(0, 0) shouldBe Some(Wall())
        grid.getCell(4, 1) shouldBe Some(Elf(3, 200))
        grid.getCell(1, 2) shouldBe Some(Goblin(3, 200))
      }
    }
  }
}
