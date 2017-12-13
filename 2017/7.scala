import scala.io.Source
import util.matching.Regex

val input = Source.fromFile("input7.txt")

implicit class RegexContext(sc: StringContext) {
  def r = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

val parsedLines = input.getLines.map{
  case r"""(\w+)${name}\s+\((\d+)${weight}\)"""                       => (name, weight, Array.empty[String])
  case r"""(\w+)${name}\s+\((\d+)${weight}\)\s+->\s*(.*)${parents}""" => (name, weight, parents.split(", "))
}.toList

val weights   = parsedLines.map{case (name, weight, _)  => (name -> weight.toInt)}.toMap
val subTowers = parsedLines.map{case (name, _, parents) => (name -> parents)}.toMap

val towers = subTowers.keySet
val allParents = subTowers.values.flatten.toSet

val answer1 = (towers -- allParents).head
println(answer1)

def subTowerWeights(tower: String): Array[Int] = {
  val subs = subTowers(tower)
  subs.map{subTower => subTowerWeights(subTower).sum + weights(subTower)}
}

def weightFreqs(tower: String): Map[Int, Int] =
  subTowerWeights(tower).groupBy(identity).mapValues(_.size)

def unbalancedWeight(tower: String): Int = weightFreqs(tower).minBy{_._2}._1
def   balancedWeight(tower: String): Int = weightFreqs(tower).maxBy{_._2}._1

def unbalancedSubTower(tower: String): String = {
  val index = subTowerWeights(tower).indexOf(unbalancedWeight(tower))
  subTowers(tower)(index)
}

def areSubTowersBalanced(tower: String): Boolean = {
  subTowerWeights(tower).distinct.size == 1
}

val unbalancedOffset = balancedWeight(answer1) - unbalancedWeight(answer1)

def unbalancedPath = Stream.iterate(answer1)(unbalancedSubTower)

val unbalancedTower = unbalancedPath.dropWhile{!areSubTowersBalanced(_)}.head

val answer2 = weights(unbalancedTower) + unbalancedOffset

println(answer2)
