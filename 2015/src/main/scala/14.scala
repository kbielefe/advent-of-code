package day14
import parse.{*, given}

case class Reindeer(name: String, speed: Int, time: Int, rest: Int):
  def distanceAfter(seconds: Int): Int =
    val fullIntervals = seconds / (time + rest)
    val partialInterval = Math.min(seconds % (time + rest), time)
    fullIntervals * time * speed + partialInterval * speed

type I = List[Reindeer ~ """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\."""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.map(_.distanceAfter(2503)).max

  def part2(input: I): Int =
    val endScores = (1 to 2503).foldLeft(Map.empty[String, Int]){(scores, second) =>
      val currentDistances = input.map(reindeer => reindeer.name -> reindeer.distanceAfter(second)).toMap
      val lead = currentDistances.values.max
      currentDistances.filter(_._2 == lead).map(_._1).foldLeft(scores)((scores, reindeer) => scores + (reindeer -> (scores.getOrElse(reindeer, 0) + 1)))
    }
    endScores.map(_._2).max
