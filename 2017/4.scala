import scala.io.Source

val input = Source.fromFile("input4.txt").getLines.map(_.split(' ')).toList

val noDups = (words: Array[String]) => words.distinct.size == words.size
val answer1 = input.filter(noDups).size
val answer2 = input.map{_ map {_.sorted}}.filter(noDups).size

println(answer1)
println(answer2)
