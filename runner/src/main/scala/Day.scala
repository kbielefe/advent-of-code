package runner
import cats.effect.IO
import cats.effect.std.Console
import parse.*

private[runner] trait NormalizedDay:
  def normalizedPart1(input: String): IO[String]
  def normalizedPart2(input: String): IO[String]

  def normalize[I, A](stringInput: String, calculate: I => IO[A])(using r: Read[I], s: Show[A]): IO[String] = for
    readInput     <- IO(r.read(stringInput))
    timedResult   <- calculate(readInput).timed
    (time, result) = timedResult
    _             <- Console[IO].println(s"${time.toMillis} ms")
    stringResult  <- IO(s.show(result))
  yield stringResult

trait Day[I: Read, A: Show, B: Show] extends NormalizedDay:
  def part1(input: I): A
  def part2(input: I): B

  override def normalizedPart1(input: String): IO[String] =
    normalize[I, A](input, i => IO.blocking(part1(i)))

  override def normalizedPart2(input: String): IO[String] =
    normalize[I, B](input, i => IO.blocking(part2(i)))

trait IODay[I: Read, A: Show, B: Show] extends NormalizedDay:
  def part1(input: I): IO[A]
  def part2(input: I): IO[B]

  override def normalizedPart1(input: String): IO[String] =
    normalize[I, A](input, i => part1(i))

  override def normalizedPart2(input: String): IO[String] =
    normalize[I, B](input, i => part2(i))
