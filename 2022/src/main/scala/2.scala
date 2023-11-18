import parse.{*, given}

enum RPS:
  case Rock, Paper, Scissors

  def shapeScore: Int = this match
    case Rock     => 1
    case Paper    => 2
    case Scissors => 3

given CanEqual[RPS, RPS] = CanEqual.derived

given Read[RPS] with
  import RPS.*
  def read(string: String): RPS =
    summon[Read[Char]].read(string) match
      case 'A' => RPS.Rock
      case 'B' => RPS.Paper
      case 'C' => RPS.Scissors
      case 'X' => RPS.Rock
      case 'Y' => RPS.Paper
      case 'Z' => RPS.Scissors

case class Round(opponent: RPS, me: RPS) derives ReadProduct:
  import RPS.*

  val Win  = 6
  val Lose = 0
  val Draw = 3

  def outcome: Int = (opponent, me) match
    case (Rock,     Rock)     => Draw
    case (Rock,     Paper)    => Win
    case (Rock,     Scissors) => Lose
    case (Paper,    Rock)     => Lose
    case (Paper,    Paper)    => Draw
    case (Paper,    Scissors) => Win
    case (Scissors, Rock)     => Win
    case (Scissors, Paper)    => Lose
    case (Scissors, Scissors) => Draw

  def score: Int = me.shapeScore + outcome
end Round

type Day2Input = List[Round - " "] - "\n"
object Day2 extends runner.Day[Day2Input, Int, Int]:
  def part1(input: Day2Input): Int =
    input.map(_.score).sum

  def part2(input: Day2Input): Int =
    ???
