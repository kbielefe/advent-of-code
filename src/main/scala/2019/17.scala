package advent2019
import common.{DayTask, Intcode}
import monix.eval.Task
import monix.reactive.Observable
import cats.effect.concurrent.{MVar, Ref}

class Day17 extends DayTask[Map[Long, Long], Int, String] {
  override def input(lines: Observable[String]) = lines.headL.map{line =>
    line.split(",").zipWithIndex.map{case (value, index) => ((index.toLong, value.toLong))}.toMap
  }

  def camera(output: MVar[Task, Long], view: Ref[Task, List[Char]]): Task[Unit] = for {
    pixel <- output.take
    _     <- view.update(pixel.toChar :: _)
    _     <- camera(output, view)
  } yield ()

  def run(initialMemory: Map[Long, Long]): Task[List[Char]] = for {
    in      <- MVar.empty[Task, Long]
    out     <- MVar.empty[Task, Long]
    view    <- Ref.of[Task, List[Char]](List.empty)
    camTask <- camera(out, view).start
    program <- new Intcode("camera", in, out).run(initialMemory, 0, 0)
    result  <- view.get
    _       <- camTask.cancel
  } yield result.reverse

  def parseMap(input: String): Map[(Int, Int), Char] = {
    val lines = input.split('\n')
    lines.zipWithIndex.flatMap{case (line, y) =>
      line.zipWithIndex.map{case (char, x) =>
        (x, y) -> char
      }
    }.toMap
  }

  def findIntersections(map: Map[(Int, Int), Char]): Iterable[(Int, Int)] = {
    map.keys.filter{case (x, y) =>
      map.getOrElse((x, y),   0) == '#' &&
      map.getOrElse((x, y+1), 0) == '#' &&
      map.getOrElse((x, y-1), 0) == '#' &&
      map.getOrElse((x-1, y), 0) == '#' && 
      map.getOrElse((x+1, y), 0) == '#'
    }
  }

  def alignmentParameters(intersections: Iterable[(Int, Int)]): Int = {
    intersections.map{case (x, y) => x * y}.sum
  }

  override def part1(initialMemory: Map[Long, Long]) =
    run(initialMemory)
      .map(_.mkString)
      .map(parseMap)
      .map(findIntersections)
      .map(alignmentParameters)

  override def part2(initialMemory: Map[Long, Long]) = Task{"unimplemented"}
}
