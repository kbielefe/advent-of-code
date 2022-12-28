package advent2022
import algorithms.branchAndBoundMaxLifo

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

  case class Robot(material: String, costs: Map[String, Int]):
    def canBuild(materials: Map[String, Int]): Boolean =
      costs.forall{case (material, cost) => materials.getOrElse(material, 0) >= cost}

  case class State(timeRemaining: Int, robots: Map[String, Robot], maxRobotCounts: Map[String, Int], robotCounts: Map[String, Int], materials: Map[String, Int]):
    override def toString: String = s"""
      |State:
      |  time remaining: $timeRemaining
      |  robot blueprints:
      |    ${robots.values.toList.sortBy(_.material).mkString("\n    ")}
      |  max robot counts:
      |    ${maxRobotCounts.toList.sorted.map((name, count) => s"$name: $count").mkString("\n    ")}
      |  current robot counts:
      |    ${robotCounts.toList.sorted.map((name, count) => s"$name: $count").mkString("\n    ")}
      |  materials:
      |    ${materials.toList.sorted.map((name, count) => s"$name: $count").mkString("\n    ")}
      |""".stripMargin

    def objective: Int =
      materials.getOrElse("geode", 0)

    def branch: Set[State] =
      val toBuild = Set("geode", "obsidian", "clay", "ore")
        .filter(material => maxRobotCounts.getOrElse(material, 0) > robotCounts.getOrElse(material, 0))
        .filter(robots(_).canBuild(materials))
      val build = if toBuild.isEmpty then
        Set.empty
      else
        toBuild.map(material => State(timeRemaining - 1, robots, maxRobotCounts, robotCounts.updated(material, robotCounts.getOrElse(material, 0) + 1), materials - robots(material).costs + robotCounts))
      val justMine = State(timeRemaining - 1, robots, maxRobotCounts, robotCounts, materials + robotCounts)
      build + justMine

    // Don't build any more robots
    def lowerBound: Int =
      materials.getOrElse("geode", 0) + timeRemaining * robotCounts.getOrElse("geode", 0)

    // Build all remaining geode robots one per turn
    def upperBound: Int =
      val existingGeodeRobots = robotCounts.getOrElse("geode", 0)
      val maxGeodeRobots = maxRobotCounts.getOrElse("geode", 0)
      val remainingGeodeRobots = Math.min(timeRemaining, Math.max(0, maxGeodeRobots - existingGeodeRobots))
      materials.getOrElse("geode", 0) +
        existingGeodeRobots * timeRemaining +
        remainingGeodeRobots * (remainingGeodeRobots - 1) / 2

    def solution: Boolean =
      timeRemaining == 0

  case class Blueprint(id: Int, robots: Map[String, Robot]):
    def qualityLevel: Int =
      id * maxGeodes

    private def maxRobotCount(material: String): Int =
      robots.values.map(_.costs.getOrElse(material, 0)).max

    private def maxGeodes: Int =
      val initialStates = for
        ore      <- (1 to maxRobotCount("ore")).iterator
        clay     <- (1 to maxRobotCount("clay")).iterator
        obsidian <- (1 to Math.min(3, maxRobotCount("obsidian"))).iterator
        geode = 24 - ore - clay - obsidian
        if geode > 0
      yield State(24, robots, Map("ore" -> ore, "clay" -> clay, "obsidian" -> obsidian, "geode" -> geode), Map("ore" -> 1), Map.empty)
      initialStates.map(branchAndBoundMaxLifo[State, Int](_.objective, _.branch, _.lowerBound, _.upperBound, _.solution)).tapEach(println).map(_._2).max

  private def parseBlueprint(input: String): Blueprint = input match
    case s"Blueprint $id: $robots" => Blueprint(id.toInt, parseRobots(robots))

  private def parseRobots(input: String): Map[String, Robot] =
    input.split("\\. ").map(parseRobot).map(robot => (robot.material, robot)).toMap

  private def parseRobot(input: String): Robot = input match
    case s"Each $material robot costs $costs" => Robot(material, parseCosts(costs))

  private def parseCosts(input: String): Map[String, Int] =
    input.split(" and ").map{
      case s"$count $material" => material.stripSuffix(".") -> count.toInt
    }.toMap
