package advent2022
import algorithms.branchAndBoundMaxFifo

object Day19:
  def part1(input: List[String]): Int =
    input.map(parseBlueprint).map(_.qualityLevel).sum

  def part2(input: List[String]): Int =
    ???

  extension (lhs: Map[String, Int])
    // Pairwise addition and subtraction
    def +(rhs: Map[String, Int]): Map[String, Int] =
      (lhs.toList ++ rhs.toList).groupMapReduce(_._1)(_._2)(_ + _)

    def -(rhs: Map[String, Int]): Map[String, Int] =
      (lhs.toList ++ rhs.view.mapValues(-_).toList).groupMapReduce(_._1)(_._2)(_ + _)

  case class State(timeRemaining: Int, materialCounts: Map[String, Int], robotCounts: Map[String, Int]):
    def objective: Int =
      materialCounts.getOrElse("geode", 0)

    def solution: Boolean =
      timeRemaining == 0

    def branch(robots: List[Robot]): Set[State] =
      val minedMaterials = robotCounts
      val canBuild = robots.filter(_.canBuild(materialCounts)).filterNot(maxCount(robots))
      val builtStates = canBuild.map(robot => State(timeRemaining - 1, materialCounts - robot.costs + minedMaterials, robotCounts + Map(robot.material -> 1)))
      builtStates.toSet + State(timeRemaining - 1, materialCounts + minedMaterials, robotCounts)

    private def maxCount(robots: List[Robot])(robot: Robot): Boolean =
      if robot.material == "geode" then
        false
      else
        robotCounts.getOrElse(robot.material, 0) >= robots.iterator.map(_.costs.getOrElse(robot.material, 0)).max

    // Don't build any more geode robots, just mine with what we've already got
    def lowerBound: Int =
      materialCounts.getOrElse("geode", 0) + timeRemaining * robotCounts.getOrElse("geode", 0)

    // How many geodes we would make if we built a geode robot every turn from now on.
    def upperBound: Int =
      materialCounts.getOrElse("geode", 0) +
        robotCounts.getOrElse("geode", 0) * (timeRemaining + 1) +
        timeRemaining * (timeRemaining + 1) / 2

  case class Robot(material: String, costs: Map[String, Int]):
    def canBuild(materials: Map[String, Int]): Boolean =
      costs.forall{case (material, cost) => materials.getOrElse(material, 0) >= cost}

  case class Blueprint(id: Int, robots: List[Robot]):
    def qualityLevel: Int =
      val initialState = State(24, Map.empty, Map("ore" -> 1))
      val (finalState, maxGeodes) = branchAndBoundMaxFifo[State, Int](_.objective, _.branch(robots), _.lowerBound, _.upperBound, _.solution)(initialState)
      println(s"$robots $finalState $maxGeodes")
      id * maxGeodes

  private def parseBlueprint(input: String): Blueprint = input match
    case s"Blueprint $id: $robots" => Blueprint(id.toInt, parseRobots(robots))

  private def parseRobots(input: String): List[Robot] =
    input.split("\\. ").map(parseRobot).toList

  private def parseRobot(input: String): Robot = input match
    case s"Each $material robot costs $costs" => Robot(material, parseCosts(costs))

  private def parseCosts(input: String): Map[String, Int] =
    input.split(" and ").map{
      case s"$count $material" => material.stripSuffix(".") -> count.toInt
    }.toMap
