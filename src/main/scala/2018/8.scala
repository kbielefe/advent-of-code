package advent2018
import common.{Day, StateUtils}
import scala.io.Source
import cats.data.State

class Day8(source: Source) extends Day {

  val input = source.mkString.trim.split(" ").map{_.toInt}.toList

  case class Node(children: List[Node], metadata: List[Int])

  val nextInt: State[List[Int], Int] = State(input => (input.tail, input.head))
  def nextNode: State[List[Int], Node] = for {
    childrenCount <- nextInt
    metadataCount <- nextInt
    children      <- StateUtils.repeatN(childrenCount)(nextNode)
    metadata      <- StateUtils.repeatN(metadataCount)(nextInt)
  } yield Node(children, metadata)

  def parseNode(input: List[Int]): Node = nextNode.runA(input).value

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
    sumMetadata(parseNode(input)).toString
  }

  override def answer2: String = {
    nodeValue(parseNode(input)).toString
  }
}
