package advent2018
import common.{Day, Grid}
import scala.io.Source

class Day15(source: Source) extends Day {
  abstract class Cell(gridChar: Char) extends Grid.Cell {
    override def char = gridChar
  }
  abstract class Creature(char: Char, attack: Int, hitPoints: Int) extends Cell(char)

  case class Wall() extends Cell('#')
  case class Elf(attack:    Int = 3, hitPoints: Int = 200) extends Creature('E', attack, hitPoints)
  case class Goblin(attack: Int = 3, hitPoints: Int = 200) extends Creature('G', attack, hitPoints)

  def charToCell(char: Char): Cell = char match {
    case '#' => Wall()
    case 'E' => Elf()
    case 'G' => Goblin()
  }

  lazy val grid = Grid(0, 0, '.', source, _ => None, charToCell _)

  override def answer1: String = grid.getLines().mkString("\n")
  override def answer2: String = ???
}
