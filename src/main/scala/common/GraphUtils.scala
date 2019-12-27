package common
import scalax.collection.edge.LDiEdge
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.util.Try
import scala.language.higherKinds
import monix.reactive.Observable
import monix.eval.Task

object GraphUtils {
  // Assumes a single successor for every node within n
  def move[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E], node: N, n: Int): N = {
    if (n == 0)
      node
    else if (n > 0) {
      val path = Iterator.iterate(g get node){node => Try(node.diSuccessors.head) getOrElse node}
      path.drop(n).next
    } else {
      val path = Iterator.iterate(g get node){node => Try(node.diPredecessors.head) getOrElse node}
      path.drop(-1 * n).next
    }
  }

  def insertAfterCircular[N](g: Graph[N,DiEdge], node: N, insertedNode: N): Graph[N,DiEdge] = {
    val pred = g get node
    val succ = Try(pred.diSuccessors.head) getOrElse pred
    val oldEdge: Param[N,DiEdge] = DiEdge(pred, succ)
    val toNew: Param[N,DiEdge] = DiEdge(pred, insertedNode)
    val fromNew: Param[N,DiEdge] = DiEdge(insertedNode, succ)
    g - oldEdge + toNew + fromNew
  }

  def deleteCircular[N](g: Graph[N,DiEdge], node: N): Graph[N,DiEdge] = {
    val n = g get node
    val pred = Try(n.diPredecessors.head) getOrElse n
    val succ = Try(n.diSuccessors.head) getOrElse n
    val bridge: Param[N,DiEdge] = DiEdge(pred, succ)
    g - node + bridge
  }

  case class Square(x: Int, y: Int, char: Char)

  def fromLines(lines: Observable[String]): Task[Graph[Square, LDiEdge]] = {
    def edgeBetween(squares: (Square, Square)) = squares._1.char != '#' && squares._2.char != '#'

    val squareLines = lines.zipWithIndex.map{case (line, y) =>
      line.zipWithIndex.map{case (char, x) => Square(x.toInt, y.toInt, char) }
    }

    val verticalEdges = squareLines.zip(squareLines.drop(1)).flatMap{case (aboveRow, belowRow) =>
      Observable.fromIterable(aboveRow.zip(belowRow))
        .filter(edgeBetween)
        .flatMap{case (above, below) => Observable(LDiEdge(above, below)("South"), LDiEdge(below, above)("North"))}
    }

    val horizontalEdges = squareLines.flatMap{row =>
      Observable.fromIterable(row.zip(row.drop(1)))
        .filter(edgeBetween)
        .flatMap{case (left, right) => Observable(LDiEdge(left, right)("East"), LDiEdge(right, left)("West"))}
    }

    (verticalEdges ++ horizontalEdges).toListL.map{edges => Graph(edges:_*)}
  }
}
