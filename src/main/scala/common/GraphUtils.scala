package common
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scala.util.Try

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
}
