package advent2016
import algorithms.AStar

object Day22:
  def part1(input: List[String]): Int =
    val nodes = input.drop(2).collect{
      case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T $p" =>
        Node(x.trim.toInt, y.trim.toInt, size.trim.toInt, used.trim.toInt, avail.trim.toInt)
    }
    nodes.combinations(2).count{case List(a, b) => a.viable(b)} +
    nodes.combinations(2).count{case List(a, b) => b.viable(a)}

  def part2(input: List[String]): Int =
    val astar = AStar[Position, Int](goal, heuristic, neighborWeight, 0, _.neighbors)
    val nodes = input.drop(1).collect{
      case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T $p" =>
        (x.toInt, y.toInt) -> Node(x.trim.toInt, y.trim.toInt, size.trim.toInt, used.trim.toInt, avail.trim.toInt)
    }.toMap
    val dataX = nodes.map(_._1._1).max
    val dataY = 0
    val (blankX, blankY) = nodes.find(_._2.used == 0).get._1
    val initialPosition = Position(dataX, dataY, blankX, blankY, nodes)
    astar.getMinCost(initialPosition).get

  case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int) derives CanEqual:
    def viable(other: Node): Boolean =
      this != other && used != 0 && used <= other.avail

  case class Position(dataX: Int, dataY: Int, blankX: Int, blankY: Int, nodes: Map[(Int, Int), Node]) derives CanEqual:
    override def equals(other: Any): Boolean =
      val otherPos = other.asInstanceOf[Position]
      otherPos.dataX == dataX && otherPos.dataY == dataY && otherPos.blankX == blankX && otherPos.blankY == blankY

    override def hashCode: Int =
      dataX.## + dataY.## + blankX.## + blankY.##

    override def toString: String =
      s"Position: ($dataX, $dataY)=${nodes(dataX -> dataY)} ($blankX, $blankY)=${nodes(blankX -> blankY)}"

    val blankPos = (blankX, blankY)
    val blankNode = nodes(blankPos)
    val dataPos = (dataX, dataY)
    val dataNode = nodes(dataPos)

    def neighbors: Set[Position] =
      Set((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap{case (xOffset, yOffset) =>
        val newBlankPos = (blankX + xOffset, blankY + yOffset)
        nodes.get(newBlankPos) match
          case Some(newBlankNode) =>
            if blankNode.avail >= newBlankNode.used then
              val newNodes = nodes
                .updated(newBlankPos, newBlankNode.copy(used = 0, avail = newBlankNode.size))
                .updated(blankPos, blankNode.copy(used = newBlankNode.used, avail = blankNode.size - newBlankNode.used))
              val newBlankX = newBlankPos._1
              val newBlankY = newBlankPos._2
              val newDataX = if newBlankPos == dataPos then blankX else dataX
              val newDataY = if newBlankPos == dataPos then blankY else dataY
              Set(Position(newDataX, newDataY, newBlankX, newBlankY, newNodes))
            else
              Set.empty
          case None =>
            Set.empty
      }

  def goal(pos: Position): Boolean = pos.dataX == 0 && pos.dataY == 0

  def heuristic(pos: Position): Int =
    pos.dataX + pos.dataY + Math.abs(pos.dataX - pos.blankX) + Math.abs(pos.dataY - pos.blankY) - 1

  def neighborWeight(start: Position, end: Position): Int = 1
