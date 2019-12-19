package advent2019
import common.{DayTask, Intcode}
import monix.eval.Task
import monix.reactive.Observable
import cats.effect.concurrent.MVar

class Day19 extends DayTask[Map[Long, Long], Long, String] {
  override def input(lines: Observable[String]) = lines.headL.map{line =>
    line.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap
  }

  def tractorBeamRange(in: MVar[Task, Long], out: MVar[Task, Long]): Task[Long] =
    Observable.range(0, 50).flatMap{x =>
      Observable.range(0, 50).mapEval{y =>
        for {
          _      <- in.put(x)
          _      <- in.put(y)
          result <- out.take
        } yield result
      }
    }.sumL

  def run(initialMemory: Map[Long, Long]): Task[Long] = for {
    in      <- MVar.empty[Task, Long]
    out     <- MVar.empty[Task, Long]
    program <- new Intcode("tractor beam", in, out).run(initialMemory, 0, 0).loopForever.start
    result  <- tractorBeamRange(in, out)
    _       <- program.cancel
  } yield result

  override def part1(initialMemory: Map[Long, Long]) = run(initialMemory)

  override def part2(initialMemory: Map[Long, Long]) = Task{"unimplemented"}
}
