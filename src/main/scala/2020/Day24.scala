package advent2020

object Day24:
  def part1(input: List[String]): Int =
    val tiles = input.map(dirs => dirToCoord(dirs))
    tiles.groupBy(identity).count(_._2.size % 2 == 1)

  def part2(input: List[String]): Int =
    ???

  @scala.annotation.tailrec
  private def dirToCoord(dirs: String, coord: (Int, Int, Int) = (0, 0, 0)): (Int, Int, Int) =
    if dirs.isEmpty then
      coord
    else if dirs.startsWith("n") || dirs.startsWith("s") then
      val dir = dirs.take(2)
      val (q, r, s) = coord
      val (q1, r1, s1) = directions(dir)
      dirToCoord(dirs.drop(2), (q + q1, r + r1, s + s1))
    else
      val dir = dirs.take(1)
      val (q, r, s) = coord
      val (q1, r1, s1) = directions(dir)
      dirToCoord(dirs.drop(1), (q + q1, r + r1, s + s1))

  private val directions = Map(
    "nw" -> (0, -1, 1),
    "ne" -> (1, -1, 0),
    "w"  -> (-1, 0, 1),
    "e"  -> (1, 0, -1),
    "sw" -> (-1, 1, 0),
    "se" -> (0, 1, -1)
  )
