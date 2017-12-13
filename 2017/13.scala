import scala.io.Source

val lines = Source.fromFile("input13.txt").getLines
val input = lines.map(_ split ": ").map{x => x(0).toInt -> x(1).toInt}.toMap

def caught(delay: Int)(layer: Int): Boolean = {
  val timeEnteredLayer = layer + delay
  val scanLength = input(layer) * 2 - 2
  val firewallPosition = timeEnteredLayer % scanLength
  firewallPosition == 0
}

def severity(layer: Int): Int = input(layer) * layer

def caughtOnAnyLayer(delay: Int): Boolean = input.keys.exists{layer => caught(delay)(layer)}

val answer1 = input.keys.filter{caught(0)}.map(severity).sum
println(answer1)

val answer2 = Stream.from(1).filterNot{caughtOnAnyLayer}.head
println(answer2)
