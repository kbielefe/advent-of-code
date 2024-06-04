package day16
import algorithms.floydWarshall
import parse.{*, given}

case class Valve(name: String, rate: Int, tunnels: List[String] - ", ") derives ReadProduct

case class Node(opened: Boolean, valve: Valve)

type I = List[Valve]
given Read[I] = Read("\n")
given Read[Valve] = Read("""Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r)

object Puzzle extends runner.Day[I, Int, Int]:
  def part1(input: I): Int =
    val (floyd, start, unvisited) = floydStartUnvisited(input)
    calculatePressure(start, unvisited, floyd, 0, 30)

  def part2(input: I): Int =
    val (floyd, start, unvisited) = floydStartUnvisited(input)
    calculateElephantPressure(start, start, unvisited, floyd, 0, 26, 0, 0)

  def floydStartUnvisited(input: I): (Map[(Node, Node), Int], Valve, Set[Valve]) =
    val valveByName = input.map(valve => valve.name -> valve).toMap
    val edges = input.flatMap {valve =>
      val neighbors = valve.tunnels.map(valveByName.apply).map(valve => Node(false, valve))
      val start = Node(false, valve)
      val opened = Node(true, valve)
      List((start, opened) -> 1, (opened, start) -> 0) ++ neighbors.flatMap {neighbor => List((start, neighbor) -> 1, (neighbor, start) -> 1)}
    }.toMap
    val floyd = floydWarshall(edges)
    val start = input.find(_.name == "AA").get
    val unvisited = input.filter(_.rate != 0).toSet.asInstanceOf[Set[Valve]]
    (floyd, start, unvisited)

  def calculatePressure(current: Valve, unvisited: Set[Valve], floyd: Map[(Node, Node), Int], totalPressure: Int, timeRemaining: Int): Int =
    if (unvisited.isEmpty) then
      totalPressure
    else
      val candidates = unvisited.map{valve =>
        val travelTime = floyd(Node(false, current) -> Node(true, valve))
        val pressure = (timeRemaining - travelTime) * valve.rate
        (valve, pressure, travelTime)
      }.filter(_._2 > 0)
      if candidates.isEmpty then
        totalPressure
      else
        candidates.map{case (valve, pressure, travelTime) =>
          calculatePressure(valve, unvisited - valve, floyd, totalPressure + pressure, timeRemaining - travelTime)
        }.max

  var maxPressure = 0

  def canPrune(unvisited: Set[Valve], totalPressure: Int, timeRemaining: Int): Boolean =
    unvisited.map(_.rate).sum * timeRemaining + totalPressure <= maxPressure

  def calculateElephantPressure(
    me: Valve,
    elephant: Valve,
    unvisited: Set[Valve],
    floyd: Map[(Node, Node), Int],
    totalPressure: Int,
    timeRemaining: Int,
    meTravelTime: Int,
    elephantTravelTime: Int
  ): Int =
    if timeRemaining <= 0 then
      maxPressure = Math.max(maxPressure, totalPressure)
      totalPressure
    else if canPrune(unvisited, totalPressure, timeRemaining) then
      0
    else if meTravelTime <= 0 then
      val candidates = unvisited.map{valve =>
        val travelTime = floyd(Node(false, me) -> Node(true, valve))
        val pressure = (timeRemaining - travelTime - meTravelTime) * valve.rate
        (valve, pressure, travelTime)
      }.filter(_._2 > 0)
      if candidates.isEmpty then
        calculateElephantPressure(
          me,
          elephant,
          unvisited,
          floyd,
          totalPressure,
          timeRemaining - 1,
          meTravelTime - 1,
          elephantTravelTime - 1
        )
      else
        candidates.map{case (valve, pressure, travelTime) =>
          calculateElephantPressure(
            valve,
            elephant,
            unvisited - valve,
            floyd,
            totalPressure + pressure,
            timeRemaining - 1,
            meTravelTime + travelTime - 1,
            elephantTravelTime - 1
          )
        }.max
    else if elephantTravelTime <= 0 then
      val candidates = unvisited.map{valve =>
        val travelTime = floyd(Node(false, elephant) -> Node(true, valve))
        val pressure = (timeRemaining - travelTime - elephantTravelTime) * valve.rate
        (valve, pressure, travelTime)
      }.filter(_._2 > 0)
      if candidates.isEmpty then
        calculateElephantPressure(
          me,
          elephant,
          unvisited,
          floyd,
          totalPressure,
          timeRemaining - 1,
          meTravelTime - 1,
          elephantTravelTime - 1
        )
      else
        candidates.map{case (valve, pressure, travelTime) =>
          calculateElephantPressure(
            me,
            valve,
            unvisited - valve,
            floyd,
            totalPressure + pressure,
            timeRemaining - 1,
            meTravelTime - 1,
            elephantTravelTime + travelTime - 1
          )
        }.max
    else
      calculateElephantPressure(
        me,
        elephant,
        unvisited,
        floyd,
        totalPressure,
        timeRemaining - 1,
        meTravelTime - 1,
        elephantTravelTime - 1
      )
