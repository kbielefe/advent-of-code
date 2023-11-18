package day19
import parse.{*, given}

case class Cost(amount: Int, material: String) derives ReadProduct
case class Robot(name: String, cost: List[Cost ~ """(\d+) (\w+)"""] - " and ") derives ReadProduct
case class Blueprint(number: Int, robots: List[Robot] ~ """Each (\w+) robot costs ([^.]+)\. ?""") derives ReadProduct

type I = List[Blueprint ~ """Blueprint (\d+): (.+)"""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.foreach(println)
    ???

  def part2(input: I): Int =
    ???
