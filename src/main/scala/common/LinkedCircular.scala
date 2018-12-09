package common

// g maps node -> (back, forward)
case class LinkedCircular[A](protected val g: Map[A, (A, A)]) {

  def move(from: A, count: Int): A = {
    if (count == 0) {
      from
    } else if (count < 0) {
      Iterator.iterate(from){g(_)._1}.drop(-1 * count).next
    } else {
      Iterator.iterate(from){g(_)._2}.drop(count).next
    }
  }

  def insertAfter(pred: A, inserted: A): LinkedCircular[A] = {
    val (predBack, succ) = g(pred)
    val (_, succForward) = g(succ)
    if (pred == succ)
      LinkedCircular(g + (pred -> (inserted, inserted)) + (inserted -> (pred, succ)))
    else
      LinkedCircular(g + (pred -> (predBack, inserted)) + (inserted -> (pred, succ)) + (succ -> (inserted, succForward)))
  }

  def delete(deleted: A): LinkedCircular[A] = {
    val (pred, succ) = g(deleted)
    val (predBack, _) = g(pred)
    val (_, succForward) = g(succ)
    if (pred == succ)
      LinkedCircular(g - deleted + (pred -> (pred, succ)))
    else
      LinkedCircular(g - deleted + (pred -> (predBack, succ)) + (succ -> (pred, succForward)))
  }

  def toList(startNode: A): List[A] = {
    Iterator.iterate(startNode){g(_)._2}.take(g.size).toList
  }
}
