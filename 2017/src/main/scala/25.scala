package advent2017
import common.Day
import scala.io.Source
import util.matching.Regex

class Day25(source: Source) extends Day {
  case class State(write: Vector[Int], move: Vector[Int], nextState: Vector[Char])

  val initialState = 'A'
  val steps = 12399302

  val states = Map(
    'A' -> State(Vector(1, 0), Vector( 1,  1), Vector('B', 'C')),
    'B' -> State(Vector(0, 0), Vector(-1,  1), Vector('A', 'D')),
    'C' -> State(Vector(1, 1), Vector( 1,  1), Vector('D', 'A')),
    'D' -> State(Vector(1, 0), Vector(-1, -1), Vector('E', 'D')),
    'E' -> State(Vector(1, 1), Vector( 1, -1), Vector('F', 'B')),
    'F' -> State(Vector(1, 1), Vector( 1,  1), Vector('A', 'E'))
  )

  def iterations = Iterator.iterate((Set.empty[Int], initialState, 0)){case (tape, stateName, position) =>
    val value = if (tape contains position) 1 else 0
    val state = states(stateName)
    val newValue = state.write(value)
    val newPosition = state.move(value) + position
    val newState = state.nextState(value)
    val newTape = if (newValue == 0) tape - position else tape + position
    (newTape, newState, newPosition)
  }

  override def answer1 = iterations.drop(1 + steps).next._1.size.toString
  override def answer2 = "free"
}
