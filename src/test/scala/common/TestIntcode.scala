package common
import org.scalatest._
import monix.eval.Task
import cats.effect.concurrent.MVar
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

class TestIntcode extends UnitSpec {
  "Intcode" when {
    "running an infinite list" should {
      "allow cancelling" in {
        val memoryList = List(
          1101, 0, 1, 1,
          4, 1,
          1105, 1, 0
        )

        val memory = memoryList
          .zipWithIndex
          .map{case (instruction, index) => ((index.toLong, instruction.toLong))}
          .toMap

        def waitForOutput(target: Long, output: MVar[Task, Long]): Task[Unit] = for {
          value <- output.take
          _     <- if (value == target) Task.unit else waitForOutput(target, output)
        } yield ()

        val interpreter = for {
          in   <- MVar.empty[Task, Long]
          out  <- MVar.empty[Task, Long]
          code <- new Intcode("test", in, out).run(memory, 0, 0).start
          _    <- waitForOutput(10, out)
          _    <- code.cancel
        } yield ()

        interpreter.runSyncUnsafe(5 seconds)
        assert(true)
      }
    }
  }
}
