import scala.io.Source

val input = Source.fromFile("input1.txt").map(_.asDigit).toList.filter{_ != -1}

def zipped(offset: Int) = input zip ((input ++ input) drop offset)
def answer(offset: Int) = zipped(offset).filter{case (x,y) => x == y}.map{_._1}.sum

println(answer(1))
println(answer(input.size/2))
