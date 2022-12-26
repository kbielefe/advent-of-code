package advent2022
import algorithms.branchAndBoundMax

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
      materialCounts.getOrElse("ore", 0) // TODO: change back to geode

    def solution: Boolean =
      timeRemaining == 0

    def branch(robots: List[Robot]): Set[State] =
      val minedMaterials = robotCounts
      val canBuild = robots.filter(_.canBuild(materialCounts))
      val builtStates = canBuild.map(robot => State(timeRemaining - 1, materialCounts - robot.costs + minedMaterials, robotCounts + Map(robot.material -> 1)))
      builtStates.toSet + State(timeRemaining - 1, materialCounts + minedMaterials, robotCounts)

    // closest estimate that can be quickly calculated and is not above the actual answer
    def lowerBound: Int =
      // Don't build any more robots, just mine with what we've already got
      materialCounts.getOrElse("ore", 0) + timeRemaining * robotCounts.getOrElse("ore", 0)

    // closest estimate that can be quickly calculated and is not below the actual answer
    def upperBound(robots: List[Robot]): Int =
      (timeRemaining to 0 by -1).foldLeft((robotCounts("ore"), materialCounts.getOrElse("ore", 0))){case ((robots, ore), time) =>
        (robots + 1, ore + robots)
      }._2

  case class Robot(material: String, costs: Map[String, Int]):
    def canBuild(materials: Map[String, Int]): Boolean =
      costs.forall{case (material, cost) => materials.getOrElse(material, 0) >= cost}

  case class Blueprint(id: Int, robots: List[Robot]):
    def qualityLevel: Int =
      val initialState = State(24, Map.empty, Map("ore" -> 1))
      val (finalState, maxGeodes) = branchAndBoundMax[State, Int](_.objective, _.branch(robots), _.lowerBound, _.upperBound(robots), _.solution)(initialState)
      println(s"$finalState $maxGeodes")
      id * maxGeodes

  private def parseBlueprint(input: String): Blueprint = input match
    case s"Blueprint $id: $robots" => Blueprint(id.toInt, parseRobots(robots))

  private def parseRobots(input: String): List[Robot] =
    input.split("\\. ").map(parseRobot).filter(_.material == "ore").toList // TODO: remove filter

  private def parseRobot(input: String): Robot = input match
    case s"Each $material robot costs $costs" => Robot(material, parseCosts(costs))

  private def parseCosts(input: String): Map[String, Int] =
    input.split(" and ").map{
      case s"$count $material" => material.stripSuffix(".") -> count.toInt
    }.toMap
