package advent2021

object Day12:
  def part1(input: List[String]): Int =
    val caves = parseCaves(input)
    dfsCount("start", caves, Set("start"), true)

  def part2(input: List[String]): Int =
    val caves = parseCaves(input)
    dfsCount("start", caves, Set("start"), false)

  type Caves = Map[String, Set[String]]

  private def parseCaves(input: List[String]): Caves =
    val pairs = input.map(_.split('-')).flatMap{case Array(x, y) => Set((x, y), (y, x))}
    pairs.groupBy(_._1).map((name, neighbors) => (name, neighbors.map(_._2).toSet)).toMap

  private def isSmall(cave: String): Boolean = cave.head.isLower

  private def dfsCount(cave: String, caves: Caves, visited: Set[String], smallVisitedTwice: Boolean): Int =
    val neighbors = if smallVisitedTwice then caves(cave) -- visited else caves(cave) - "start"
    if cave == "end" then
      1
    else if neighbors.isEmpty then
      0
    else
      val newVisited = if isSmall(cave) then visited + cave else visited
      neighbors.toList.map{neighbor =>
        val newSmallVisitedTwice = smallVisitedTwice || (isSmall(neighbor) && visited.contains(neighbor))
        dfsCount(neighbor, caves, newVisited, newSmallVisitedTwice)
      }.sum
  end dfsCount

end Day12
