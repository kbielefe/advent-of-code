import scala.io.Source

type Bridge     = List[(Int, Int)]
type Components =  Set[(Int, Int)]

val rawInput = Source.fromFile("input24.txt").getLines.map{_.split("/").map(_.toInt)}.toSet

val input = rawInput.flatMap{case Array(x, y) => Set(Array(x, y), Array(y, x))}.groupBy{_(0)}.mapValues(_.map(_.sorted).map{case Array(x, y) => (x, y)})

def removeElement(element: (Int, Int), input: Map[Int, Components]): Map[Int, Components] = {
  val (x, y) = element
  val newFirstSet = input(x) - element
  val afterFirstRemoved = if (newFirstSet.isEmpty) input - x else input + (x -> newFirstSet)
  val newSecondSet = input(y) - element
  if (newSecondSet.isEmpty) afterFirstRemoved - y else afterFirstRemoved + (y -> newSecondSet)
}

def bridges(input: Map[Int, Components], acc: Bridge = List.empty[(Int, Int)])(port: Int): Stream[Bridge] = {
  if (input contains port) {
    input(port).toStream
      .flatMap{case (x, y) =>
        val next = bridges(removeElement((x, y), input), (x, y) :: acc) _
        if (x == port) next(y) else next(x)
      }
  } else {
    Stream(acc.reverse)
  }
}

def strength(bridge: Bridge): Int = bridge.map{case (x, y) => x + y}.sum

val memBridge = bridges(input)(0)

val answer1 = memBridge.map(strength).max
println(answer1)

val longestLength = memBridge.map{_.length}.max
val answer2 = memBridge.filter{_.length == longestLength}.map(strength).max
println(answer2)
