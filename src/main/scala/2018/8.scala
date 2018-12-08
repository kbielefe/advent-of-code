package advent2018
import common.Day
import scala.io.Source

class Day8(source: Source) extends Day {

  val input = source.mkString.trim.split(" ").map{_.toInt}.toList

  case class Node(children: List[Node], metadata: List[Int])

  def parseNode(input: List[Int]): (Node, List[Int]) = {
    val childrenCount :: metadataCount :: content = input
    val (children, metaInput) = (1 to childrenCount).foldLeft((List.empty[Node], content)){case ((result, input), _) =>
      val (node, newInput) = parseNode(input)
      (node :: result, newInput)
    }
    val metadata = metaInput take metadataCount
    val newInput = metaInput drop metadataCount
    (Node(children.reverse, metadata), newInput)
  }

  def sumMetadata(node: Node): Int = {
    node.metadata.sum + node.children.map(sumMetadata).sum
  }

  def nodeValue(node: Node): Int = {
    if (node.children.isEmpty)
      sumMetadata(node)
    else {
      node.metadata.map{n => if (n > node.children.size) 0 else nodeValue(node.children(n - 1))}.sum
    }
  }

  override def answer1: String = {
    sumMetadata(parseNode(input)._1).toString
  }

  override def answer2: String = {
    nodeValue(parseNode(input)._1).toString
  }
}
