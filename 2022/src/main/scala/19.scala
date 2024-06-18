package day19

import parse.{*, given}
import com.google.ortools.Loader
import com.google.ortools.linearsolver.MPSolver

case class Cost(amount: Int, material: String)
given Read[Cost] = Read("""(\d+) (\w+)""".r)
given lc: Read[List[Cost]] = Read(" and ")
case class Robot(name: String, costs: List[Cost])
given Read[Robot] = Read("""Each (\w+) robot costs ([^.]+)""".r)
given Read[List[Robot]] = Read("""\.\s+""")
case class Blueprint(number: Int, robots: List[Robot]):
  def quality: Int = number * geodeCount(24)

  def geodeCount(minutes: Int): Int =
    Loader.loadNativeLibraries()
    val solver = MPSolver.createSolver("SCIP")

    val materials = List("ore", "clay", "obsidian", "geode")
    val minuteStarts = 1 to minutes

    val keys = for
      material    <- materials
      minuteStart <- minuteStarts
    yield (material, minuteStart)

    val vars = keys.map{case key@(material, minuteStart) =>
      val initial = if material == "ore" then 1 else 0
      key -> solver.makeIntVar(initial, initial + minuteStart - 1, s"$material,$minuteStart")
    }.toMap

    val objective = solver.objective()
    minuteStarts.foreach(minuteStart => objective.setCoefficient(vars("geode" -> minuteStart), 1))
    objective.setMaximization()

    val turns = minuteStarts.dropRight(1)

    turns.foreach{turnStart =>
      val turnEnd = turnStart + 1

      val totalRobotsIncrease = solver.makeConstraint(0, 1, s"$turnStart total robots increase")

      materials.foreach{material =>
        totalRobotsIncrease.setCoefficient(vars(material -> turnStart), -1)
        totalRobotsIncrease.setCoefficient(vars(material -> turnEnd),    1)

        val materialRobotsIncrease = solver.makeConstraint(0, 1, s"$turnStart $material robots increase")
        materialRobotsIncrease.setCoefficient(vars(material -> turnStart), -1)
        materialRobotsIncrease.setCoefficient(vars(material -> turnEnd),    1)
      }

      materials.filter(_ != "geode").foreach{material =>
        // Don't charge for the ore robot in your backpack
        val lowerBound = if material == "ore" then -1 * costOfMaterialPerRobot("ore", "ore") else 0

        val materialProducedMinusRequired =
          solver.makeConstraint(lowerBound, Double.PositiveInfinity, s"$turnStart $material produced - required")

        // Material Produced
        (1 until turnStart).foreach(turn => materialProducedMinusRequired.setCoefficient(vars(material -> turn), 1))

        // Material Required
        materials.foreach{robot =>
          materialProducedMinusRequired.setCoefficient(vars(robot -> turnEnd), -1 * costOfMaterialPerRobot(material, robot))
        }
      }
    }

    solver.solve()
    objective.value().toInt
  end geodeCount

  def costOfMaterialPerRobot(material: String, robot: String): Int =
    robots.find(_.name == robot).get.costs.find(_.material == material).map(_.amount).getOrElse(0)
end Blueprint

type I = List[Blueprint]
given Read[Blueprint] = Read("""Blueprint (\d+): (.+)""".r)
given Read[I] = Read("\n")

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    input.map(_.quality).sum

  def part2(input: I): Int =
    input.take(3).map(_.geodeCount(32)).product
