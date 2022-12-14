package advent2022
import scala.annotation.tailrec

object Day14:
  def part1(input: List[String]): Int =
    val obstacles = initialObstacles(input)
    val bottom = obstacles.map(_._2).max
    val initialSize = obstacles.size
    val finalSize = finalObstacles(obstacles, bottom).size
    finalSize - initialSize

  def part2(input: List[String]): Int =
    val obstacles = initialObstacles(input)
    val bottom = obstacles.map(_._2).max
    val floor = initialObstacles(List(s"${500 - bottom - 10},${bottom + 2} -> ${500 + bottom + 10},${bottom + 2}"))
    val initialSize = obstacles.size + floor.size
    val finalSize = finalObstacles(obstacles ++ floor, bottom + 2).size
    finalSize - initialSize + 1

  @tailrec
  private def finalObstacles(obstacles: Set[(Int, Int)], bottom: Int): Set[(Int, Int)] =
    addSand(obstacles, bottom) match
      case Some(restingPlace) => finalObstacles(obstacles + restingPlace, bottom)
      case None => obstacles

  @tailrec
  private def addSand(obstacles: Set[(Int, Int)], bottom: Int, moving: (Int, Int) = (500, 0)): Option[(Int, Int)] =
    val (x, y) = moving
    if y == bottom then
      None
    else if !obstacles.contains((x, y + 1)) then
      addSand(obstacles, bottom, (x, y + 1))
    else if !obstacles.contains((x - 1, y + 1)) then
      addSand(obstacles, bottom, (x - 1, y + 1))
    else if !obstacles.contains((x + 1, y + 1)) then
      addSand(obstacles, bottom, (x + 1, y + 1))
    else if y == 0 then
      None
    else
      Some(moving)

  private val start = """(\d+),(\d+)""".r
  private val continue = """ -> (\d+),(\d+)""".r

  private def initialObstacles(input: List[String]): Set[(Int, Int)] =
    input.foldLeft(Set.empty[(Int, Int)]){(obstacles, line) =>
      val first = Iterator(start.findFirstMatchIn(line).get.subgroups)
      val matches = continue.findAllMatchIn(line).map(_.subgroups)
      val coords = (first ++ matches).map{case List(x, y) => x.toInt -> y.toInt}
      coords.sliding(2).flatMap{case Seq((a,b), (x,y)) =>
        for
          x <- Math.min(a, x) to Math.max(a, x)
          y <- Math.min(b, y) to Math.max(b, y)
        yield (x, y)
      }.toSet ++ obstacles
    }
