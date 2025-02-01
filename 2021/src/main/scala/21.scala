package advent2021
import cats.*
import cats.data.*
import cats.implicits.*
import kbielefe.puzzle.{*, given}

object Day21:
  def part1(input: List[String]): Long =
    val List(player1Start, player2Start) = input.map(_.split(" ").last.toInt)
    val initial = Game(1, 0, Player(player1Start, 0), Player(player2Start, 0), 1, 1000)
    game.runA(initial).value

  def part2(input: List[String]): Long =
    val List(player1Start, player2Start) = input.map(_.split(" ").last.toInt)
    val initial = Game(1, 0, Player(player1Start, 0), Player(player2Start, 0), 1, 21)
    initial.mapReduce[(Long, Long)](_.result){case ((a, b), (x, y)) => (a + x, b + y)}.get.toList.max

  given Graph[Game] with
    def neighbors(game: Game): Iterator[Game] = game.nextMoves

  def allRolls = for
    r1 <- Iterator(1, 2, 3)
    r2 <- Iterator(1, 2, 3)
    r3 <- Iterator(1, 2, 3)
  yield r1 + r2 + r3

  def turn: State[Game, Unit] = for
    r1 <- roll
    r2 <- roll
    r3 <- roll
    _  <- move(r1 + r2 + r3)
  yield ()

  def game2(roll: Int): State[Game, Long] = turn.untilM_(winner) >> answer
  def game: State[Game, Long] = turn.untilM_(winner) >> answer
  def winner: State[Game, Boolean] = State.inspect(_.winner)
  def answer: State[Game, Long] = State.inspect(_.answer)

  def roll: State[Game, Int] = State(_.roll)
  def move(spaces: Int): State[Game, Unit] = State.modify(_.move(spaces))

  case class Player(space: Int, score: Long):
    def move(spaces: Int): Player =
      val newSpace = (space + spaces - 1) % 10 + 1
      val newScore = score + newSpace
      Player(newSpace, newScore)

  case class Game(die: Int, rolls: Int, p1: Player, p2: Player, turn: Int, target: Int):
    def answer: Long = rolls * score
    def score: Long = if turn == 1 then p1.score else p2.score
    def winner: Boolean = p1.score >= target || p2.score >= target
    def roll: (Game, Int) = (copy(die = die % 100 + 1, rolls = rolls + 1), die)
    def move(spaces: Int): Game =
      if turn == 1 then
        copy(p1 = p1.move(spaces), turn = 2)
      else
        copy(p2 = p2.move(spaces), turn = 1)
    def nextMoves: Iterator[Game] = allRolls.map(move)
    def result: Option[(Long, Long)] =
      if winner && turn == 1 then
        Some(1, 0)
      else if winner && turn == 2 then
        Some(0, 1)
      else
        None
