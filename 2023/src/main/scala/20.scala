package day20
import algorithms.detectCycle
import parse.{*, given}
import scala.annotation.tailrec
import scala.collection.immutable.Queue

enum Signal derives CanEqual:
  case Low, High

sealed trait Module:
  val name: String
  val outputs: List[String]
  def receive(source: String, signal: Signal): (Module, List[String], Signal)

case class Conjunction(override val name: String, override val outputs: List[String], inputs: Map[String, Signal]) extends Module:
  def receive(source: String, signal: Signal): (Module, List[String], Signal) =
    val newInputs = if name == "ns" then
      if signal == Signal.High then inputs + (source -> signal) else inputs
    else
      inputs + (source -> signal)
    val output = if newInputs.forall(_._2 == Signal.High) then Signal.Low else Signal.High
    (copy(inputs=newInputs), outputs, output)

case class Broadcaster(override val outputs: List[String]) extends Module:
  override val name = "broadcaster"
  def receive(source: String, signal: Signal): (Module, List[String], Signal) =
    (this, outputs, signal)

case class FlipFlop(override val name: String, override val outputs: List[String], val on: Boolean) extends Module:
  def receive(source: String, signal: Signal): (Module, List[String], Signal) =
    if signal == Signal.Low then
      if on then
        (copy(on=false), outputs, Signal.Low)
      else
        (copy(on=true), outputs, Signal.High)
    else
      (this, List.empty, Signal.Low)

case class Rx(finished: Boolean) extends Module:
  override val name = "rx"
  override val outputs: List[String] = List.empty
  def receive(source: String, signal: Signal): (Module, List[String], Signal) =
    if signal == Signal.Low then
      (Rx(true), List.empty, Signal.Low)
    else
      (this, List.empty, Signal.Low)

given Read[Module] with
  def read(input: String): Module =
    val decoded = input.replaceAll("&gt;", ">").replaceAll("&amp;", "&")
    val outputs = decoded.split(" -> ")(1).split(", ").toList
    val name = decoded.drop(1).takeWhile(_ != ' ')
    if decoded.startsWith("&") then
      Conjunction(name, outputs, Map.empty)
    else if decoded.startsWith("%") then
      FlipFlop(name, outputs, false)
    else
      Broadcaster(outputs)

type I = List[Module]
given Read[I] = Read("\n")

case class State(lowPulses: Int, highPulses: Int, modulesByName: Map[String, Module]):
  def pushButton: State =
    val queue = Queue(("button", List("broadcaster"), Signal.Low))
    Iterator.iterate((this, queue))(processSignal.tupled).dropWhile(!_._2.isEmpty).next._1

  def result: Int =
    lowPulses * highPulses

  def processSignal(state: State, queue: Queue[(String, List[String], Signal)]): (State, Queue[(String, List[String], Signal)]) =
    val ((source, dests, signal), remaining) = queue.dequeue
    val outputs = dests.filter(state.modulesByName.contains).map(dest => (dest -> state.modulesByName(dest).receive(source, signal)))
    val newModules = state.modulesByName ++ outputs.map(output => (output._1 -> output._2._1))
    val newSignals = outputs.map(output => (output._1, output._2._2, output._2._3))
    val newQueue = remaining.enqueueAll(newSignals)
    val newLow = if signal == Signal.Low then state.lowPulses + dests.size else state.lowPulses
    val newHigh = if signal == Signal.High then state.highPulses + dests.size else state.highPulses
    (State(newLow, newHigh, newModules), newQueue)

object Puzzle extends runner.Day[I, Long, Long]:
  def part1(input: I): Long =
    val inputs = input.flatMap(module => module.outputs.map(output => (output -> module.name))).groupMap(_._1)(_._2)
    val withInputs = input.map{
      case Conjunction(name, outputs, _) => Conjunction(name, outputs, inputs(name).map(_ -> Signal.Low).toMap)
      case other => other
    }
    val modulesByName = withInputs.map(module => (module.name -> module)).toMap
    val state = State(0, 0, modulesByName)
    Iterator.iterate(state)(_.pushButton).drop(1000).next.result

  def part2(input: I): Long =
    val inputs = input.flatMap(module => module.outputs.map(output => (output -> module.name))).groupMap(_._1)(_._2)
    val withInputs = input.map{
      case Conjunction(name, outputs, _) => Conjunction(name, outputs, inputs(name).map(_ -> Signal.Low).toMap)
      case other => other
    }
    val modulesByName = withInputs.map(module => (module.name -> module)).toMap + ("rx" -> Rx(false))
    val state = State(0, 0, modulesByName)
    val initials = inputs("ns").map{input =>
      val Some((initial, _)) = detectCycle(Iterator.iterate(state)(_.pushButton).map(_.modulesByName("ns").asInstanceOf[Conjunction].inputs(input)), numReps = 1, _ == Signal.High): @unchecked
      initial
    }
    initials.map(_.toLong).product
