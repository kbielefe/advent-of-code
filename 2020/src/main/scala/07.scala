package advent2020

import common._
import monix.eval.Task
import monix.reactive.Observable

object Day7 extends StringsDay[Int, Int](2020, 7) {
  private case class BagRule(outside: String, inside: List[(Int, String)])

  private def parseBagRule(rule: String): BagRule = {
    val Array(root, contained) = rule.split(" bags contain ")
    if (contained == "no other bags.")
      BagRule(root, List.empty)
    else {
      val regex = """(\d+) (.*) bags?\.?""".r
      val containedList = contained.split(", ").toList
      val containedTuples = containedList.map{_ match {
        case regex(count, name) => (count.toInt, name)
      }}
      BagRule(root, containedTuples)
    }
  }

  private def reverseTree(rules: List[BagRule]): Map[String, List[String]] =
    rules.foldLeft(Map.empty[String, List[String]]){case (tree, rule) =>
      val newNodes = rule.inside.map{case (_, name) => (name, tree.get(name).map(rule.outside :: _).getOrElse(rule.outside :: Nil))}
      tree ++ newNodes
    }

  private def forwardTree(rules: List[BagRule]): Map[String, List[(Int, String)]] =
    rules.foldLeft(Map.empty[String, List[(Int, String)]]){case (tree, rule) => tree + (rule.outside -> rule.inside)}

  private def countUniqueDescendants(root: String)(tree: Map[String, List[String]]): Set[String] =
    if (tree.getOrElse(root, List.empty).isEmpty)
      Set(root)
    else
      Set(root) ++ tree(root).map(countUniqueDescendants(_)(tree)).reduce(_ ++ _)

  private def totalBags(count: Int, root: String)(tree: Map[String, List[(Int, String)]]): Int =
    if (tree.getOrElse(root, List.empty).isEmpty)
      count
    else
      count + count * tree(root).map{case (count, child) => totalBags(count, child)(tree)}.sum

  override def part1(input: Observable[String]): Task[Int] =
    input.map(parseBagRule).toListL.map(reverseTree).map(countUniqueDescendants("shiny gold")).map(_.size - 1)

  override def part2(input: Observable[String]): Task[Int] =
    input.map(parseBagRule).toListL.map(forwardTree).map(totalBags(1, "shiny gold")).map(_ - 1)
}
