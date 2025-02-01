package advent2022
import scala.annotation.tailrec

object Day14:
  def part1(input: List[String]): Int = answer(input, false)
  def part2(input: List[String]): Int = answer(input, true) + 1

  private def answer(input: List[String], floor: Boolean): Int =
    val obstacles = initialObstacles(input)
    val bottom = obstacles.map(_._2).max + (if floor then 1 else 0)

    @tailrec
    def finalObstacles(obstacles: Set[(Int, Int)]): Set[(Int, Int)] =
      addSand(obstacles) match
        case Some(restingPlace) => finalObstacles(obstacles + restingPlace)
        case None => obstacles

    @tailrec
    def addSand(obstacles: Set[(Int, Int)], moving: (Int, Int) = (500, 0)): Option[(Int, Int)] =
      val (x, y) = moving
      if y == bottom then
        if floor then Some(moving) else None
      else if !obstacles.contains((x, y + 1)) then
        addSand(obstacles, (x, y + 1))
      else if !obstacles.contains((x - 1, y + 1)) then
        addSand(obstacles, (x - 1, y + 1))
      else if !obstacles.contains((x + 1, y + 1)) then
        addSand(obstacles, (x + 1, y + 1))
      else if y == 0 then
        None
      else
        Some(moving)

    finalObstacles(obstacles).size - obstacles.size
  end answer

  private def initialObstacles(input: List[String]): Set[(Int, Int)] =
    input.foldLeft(Set.empty[(Int, Int)]){(obstacles, line) =>
      val coords = line.split(" -> ").map(_.split(",").map(_.toInt))
      coords.sliding(2).flatMap{case Array(Array(a, b), Array(x, y)) =>
        for
          x <- Math.min(a, x) to Math.max(a, x)
          y <- Math.min(b, y) to Math.max(b, y)
        yield (x, y)
      }.toSet ++ obstacles
    }
