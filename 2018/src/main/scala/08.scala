package advent2018
import common.{Day, StateUtils}
import scala.io.Source
import cats.data.State

class Day8(source: Source) extends Day {

  val input = source.mkString.trim.split(" ").map{_.toInt}.toList

  case class Node(children: List[Node], metadata: List[Int]) {
    def metadataSum: Int = metadata.sum + children.map(_.metadataSum).sum

    def value: Int =
      if (children.isEmpty)
        metadataSum
      else {
        metadata.filter{_ <= children.size}.map{n => children(n - 1).value}.sum
      }
  }

  val nextInt: State[List[Int], Int] = State(input => (input.tail, input.head))
  def nextNode: State[List[Int], Node] = for {
    childrenCount <- nextInt
    metadataCount <- nextInt
    children      <- StateUtils.repeatN(childrenCount)(nextNode)
    metadata      <- StateUtils.repeatN(metadataCount)(nextInt)
  } yield Node(children, metadata)

  def parseNode(input: List[Int]): Node = nextNode.runA(input).value

  override def answer1: String = parseNode(input).metadataSum.toString
  override def answer2: String = parseNode(input).value.toString
}
