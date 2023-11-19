package day16
import algorithms.floydWarshall
import parse.{*, given}
import com.google.ortools.Loader
import com.google.ortools.constraintsolver.*

extension [A](i: Iterator[A])
  def takeUntil(p: A => Boolean): Iterator[A] =
    val (prefix, suffix) = i.span(x => !p(x))
    prefix ++ suffix.take(1)

extension(routing: RoutingModel)
  def iterator(vehicle: Int, solution: Assignment): Iterator[Long] =
    Iterator
      .iterate(routing.start(vehicle))(i => solution.value(routing.nextVar(i)))
      .takeUntil(routing.isEnd)

case class Valve(name: String, rate: Int, tunnels: List[String] - ", ") derives ReadProduct

case class Vertex(index: Int, opened: Boolean, valve: Valve)

type I = List[Valve ~ """Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnels? leads? to valves? (.+)"""] - "\n"

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    val valveByName = input.map(valve => valve.name -> valve).toMap
    val nodeByValveAndOpened = input.flatMap { valve =>
      List((valve, false), (valve, true))
    }.zipWithIndex.toMap
    val valveAndOpenedByNode = nodeByValveAndOpened.map{case ((valve, opened), node) => (node, (valve, opened))}.toMap
    val edges = input.flatMap {valve =>
      val neighbors = valve.tunnels.map(valveByName.apply).map(valve => nodeByValveAndOpened(valve -> false))
      val start = nodeByValveAndOpened(valve -> false)
      val opened = nodeByValveAndOpened(valve -> true)
      List((start, opened) -> 1, (opened, start) -> 0) ++ neighbors.flatMap {neighbor => List((start, neighbor) -> 1, (neighbor, start) -> 1)}
    }.toMap
    val floyd = floydWarshall(edges)
    Loader.loadNativeLibraries()
    val manager = new RoutingIndexManager(input.size * 2, 1, input.indexWhere(_.name == "AA") * 2)
    val routing = new RoutingModel(manager)
    val transitCallbackIndex = routing.registerTransitCallback{(fromIndex, toIndex) =>
      val fromNode = manager.indexToNode(fromIndex)
      val toNode = manager.indexToNode(toIndex)
      val (valve, opened) = valveAndOpenedByNode(toNode)
      if valve.name == "AA" && !opened then
        0
      else
        floyd(fromNode -> toNode)
    }
    routing.setArcCostEvaluatorOfAllVehicles(transitCallbackIndex)
    val searchParameters = main.defaultRoutingSearchParameters()
      .toBuilder()
      .setFirstSolutionStrategy(FirstSolutionStrategy.Value.PATH_CHEAPEST_ARC)
      .build()

    val solution = routing.solveWithParameters(searchParameters)

    println(s"Objective: ${solution.objectiveValue()}")
    routing.iterator(0, solution).map(manager.indexToNode).map(node => valveAndOpenedByNode(node) -> node).map{case ((valve, opened), node) => s"${valve.name}-${if opened then "o" else "c"}-$node"}.foreach(println)
    ???

  def part2(input: I): Int =
    ???
