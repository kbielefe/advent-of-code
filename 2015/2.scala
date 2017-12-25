import scala.io.Source

def input = Source.fromFile("input2.txt").getLines.map(_.split("x").map(_.toInt).sorted)

def paperSizes = input.map{case Array(x, y, z) => 3 * x * y + 2 * x * z + 2 * y * z}

val answer1 = paperSizes.sum

println(answer1)

def ribbonSizes = input.map{case Array(x, y, z) => 2 * x + 2 * y + x * y * z}

val answer2 = ribbonSizes.sum
println(answer2)
