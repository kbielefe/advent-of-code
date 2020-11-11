package common
import cats.effect._
import cats.implicits._
import monix.eval._

object Runner extends TaskApp {
  def run(args: List[String]): Task[ExitCode] =
    Task(println("Hello world!")).as(ExitCode.Success)
}
