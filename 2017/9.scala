import scala.io.Source
import Math.max

val input = Source.fromFile("input9.txt").toStream

val cancel = input.scanLeft(0){case (cancel, char) =>
  if (cancel < 2 && char == '!') 2 else max(0, cancel - 1)
}.drop(1).map(_ > 0)

val nonCanceled = (cancel zip input) filterNot {_._1} map {_._2}

val isGarbage = nonCanceled.scanLeft(false){case (garbage, char) =>
  if (char == '>') false else char == '<' || garbage
}

val nonGarbage = (isGarbage zip nonCanceled) filterNot {_._1} map {_._2} filter {_ != '<'}

val braces = nonGarbage filter {_ != ','}

val nesting = braces.scanLeft(0){case (nesting, char) => if (char == '{') nesting + 1 else nesting - 1}.drop(1)

val answer1 = (nesting zip braces).filter{_._2 == '{'}.map(_._1).sum
println(answer1)

val answer2 = (isGarbage zip nonCanceled).filter{case (isGarbage, char) => isGarbage && (char != '>')}.size
println(answer2)
