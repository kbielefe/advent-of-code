package advent2016
import algorithms.AStar

object Day22:
  def part1(input: List[String]): Int =
    val nodes = input.drop(1).collect{
      case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T $p" =>
        Node(size.trim.toInt, used.trim.toInt, avail.trim.toInt)
    }
    nodes.combinations(2).count{case List(a, b) => a.viable(b)} +
    nodes.combinations(2).count{case List(a, b) => b.viable(a)}

  def part2(input: List[String]): Int =
    val astar = AStar[Position, Int](goal, heuristic, neighborWeight, 0, _.neighbors)
    val nodes = input.drop(1).collect{
      case s"/dev/grid/node-x$x-y$y ${size}T ${used}T ${avail}T $p" =>
        (x.toInt, y.toInt) -> Node(size.trim.toInt, used.trim.toInt, avail.trim.toInt)
    }.toMap
    val dataX = nodes.map(_._1._1).max
    val dataY = 0
    val (blankX, blankY) = nodes.find(_._2.used == 0).get._1
    val initialPosition = Position(dataX, dataY, blankX, blankY, nodes)
    astar.getMinCost(initialPosition).get

  case class Node(size: Int, used: Int, avail: Int) derives CanEqual:
    def viable(other: Node): Boolean =
      other != this && used != 0 && used <= other.avail

  case class Position(dataX: Int, dataY: Int, blankX: Int, blankY: Int, nodes: Map[(Int, Int), Node]) derives CanEqual:
    override def toString: String =
      s"Position: ($dataX, $dataY)=${nodes(dataX -> dataY)} ($blankX, $blankY)=${nodes(blankX -> blankY)}"

    def neighbors: Set[Position] =
      Set((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap{case (xOffset, yOffset) =>
        val newBlank = (blankX + xOffset, blankY + yOffset)
        nodes.get(newBlank) match
          case Some(newBlankNode) =>
            val blankNode = nodes((blankX, blankY))
            if blankNode.avail >= newBlankNode.used then
              val newNodes =
                nodes -
                (blankX -> blankY) -
                newBlank +
                (newBlank -> (newBlankNode.copy(used = 0, avail = newBlankNode.size))) +
                ((blankX, blankY) -> (blankNode.copy(used = newBlankNode.used, avail = blankNode.size - newBlankNode.used)))
              val newBlankX = newBlank._1
              val newBlankY = newBlank._2
              val newDataX = if newBlankX == dataX && newBlankY == dataY then blankX else dataX
              val newDataY = if newBlankX == dataX && newBlankY == dataY then blankY else dataY
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
