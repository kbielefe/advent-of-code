package common
import scala.io.Source
import monix.tail.Iterant
import cats.Applicative
import cats.effect.Sync
import scala.language.higherKinds
import scala.collection.immutable.Queue

// Internal coordinates have (x, y) where (0, 0) is at top left
class Grid[Cell](private val zorders: Map[(Int, Int), List[Cell]]) {
  def size: Int = zorders.size

  def move(from: (Int, Int), to: (Int, Int)): Grid[Cell] = {
    val (x, y) = from
    getCell(x, y) map {c => delete(from).add(to, c)} getOrElse this
  }

  def replace(coords: (Int, Int), cell: Cell): Grid[Cell] = {
    delete(coords).add(coords, cell)
  }

  def add(coords: (Int, Int), cell: Cell): Grid[Cell] = {
    val z = zorders.getOrElse(coords, List.empty[Cell])
    val newZorders = zorders + ((coords, cell :: z))
    new Grid(newZorders)
  }

  def delete(coords: (Int, Int)): Grid[Cell] = {
    val z = zorders.getOrElse(coords, List.empty[Cell])
    if (z.isEmpty) {
      this
    } else if (z.tail.isEmpty) {
      val newZorders = zorders - coords
      new Grid(newZorders)
    } else {
      val newZorders = zorders + ((coords, z.tail))
      new Grid(newZorders)
    }
  }

  // return a new grid with just the top of the zorder that matches the predicate
  // and the next layer down for elements that are deleted
  def filter(p: (Cell) => Boolean): Grid[Cell] = {
    val (keepZorders, deleteZorders) = zorders.partition(x => p(x._2.head))
    val toKeep = keepZorders.mapValues{_.head :: Nil}
    val (toTail, toDelete) = deleteZorders.partition{_._2.size > 1}
    val tailed = toTail.mapValues{_.tail}
    val deleted = toDelete.keySet
    val newZorders = toKeep -- deleted ++ tailed
    new Grid(newZorders)
  }

  def readingOrder(p: (Cell) => Boolean = _ => true): List[(Int, Int)] = {
    zorders.toList.filter{order => p(order._2.head)}.map{_._1}.sortBy{case (x, y) => (y, x)}
  }

  /**
   * Runs turn for every cell in the cellOrder, which order is recalculated at the end of every round.
   */
  def turns[F[_], A, B](turn: (Grid[Cell], B) => (Grid[Cell], A),
                     cellOrder: (Grid[Cell]) => List[B])(
                     implicit F: Sync[F]
                     ): Iterant[F, (Grid[Cell], A, Int)] = {
    def recurse(grid: Grid[Cell], order: List[B], rounds: Int): Iterant[F, (Grid[Cell], A, Int)] = {
      if (order.isEmpty) {
        recurse(grid, cellOrder(grid), rounds + 1)
      } else {
        val (coords :: remainingOrder) = order
        val (newGrid, result) = turn(grid, coords)
        Iterant.pure((newGrid, result, rounds)) ++ recurse(newGrid, remainingOrder, rounds)
      }
    }
    recurse(this, List.empty, -1)
  }

  def getCell(x: Int, y: Int): Option[Cell] = zorders.get(x, y) map {_.head}

  def getAllCoords: Set[(Int, Int)] = zorders.keySet

  def getStack(x: Int, y: Int): List[Cell] = zorders.getOrElse((x, y), List.empty[Cell])

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Grid[Cell]]) {
      false
    } else {
      other.asInstanceOf[Grid[Cell]].zorders == zorders
    }
  }

  override def hashCode(): Int = zorders.hashCode()

  //TODO: print coordinates
  def getLines(empty: Char = ' ', cellToChar: (Cell) => Char = _ => '+'): Iterator[String] = {
    if (zorders.isEmpty) {
      Iterator.empty
    } else {
      val top    = zorders.keySet.map{_._2}.min
      val bottom = zorders.keySet.map{_._2}.max
      val left   = zorders.keySet.map{_._1}.min
      val right  = zorders.keySet.map{_._1}.max

      def getChar(x: Int, y: Int) = getCell(x, y) map {cellToChar(_)} getOrElse empty
      def getLine(y: Int) = (left to right).map(getChar(_, y)).mkString
      (top to bottom).iterator map getLine
    }
  }
}

object Grid {
  def apply[C](
    top:    Int,
    left:   Int,
    empty:  Char,
    source: Source,
    under:  Char => Option[Char],
    toCell: Char => C): Grid[C] = {
      val (_, zorders) = source.getLines.foldLeft((top, Map.empty[(Int, Int), List[C]])){case ((y, zorders), line) =>
        (y + 1, zorders ++ parseRow(y, left, empty, line, under, toCell))
      }
      new Grid(zorders)
    }

  def apply[C](
    top:    Int,
    left:   Int,
    empty:  Char,
    string: String,
    under:  Char => Option[Char],
    toCell: Char => C): Grid[C] = apply(top, left, empty, Source.fromString(string), under, toCell)

  private def parseRow[C](
    y:      Int,
    left:   Int,
    empty:  Char,
    string: String,
    under:  Char => Option[Char],
    toCell: Char => C): Map[(Int, Int), List[C]] = {
      val indexedString = string.zipWithIndex.map{case (s, i) => (s, i + left)}

      indexedString.foldLeft(Map.empty[(Int, Int), List[C]]){case (zorders, (char, x)) =>
        if (char == empty) {
          zorders
        } else {
          val zorder = Iterator.iterate(Some(char): Option[Char]){_ flatMap under}
            .takeWhile{_.isDefined}
            .map{_.get}
            .flatMap(x => if (x == empty) None else Some(toCell(x)))
            .toList

            zorders + (((x, y), zorder))
        }
      }
    }

  /*
   * Take a start point and a function that computes the reachable neighbors of a point.
   * Return an Iterant of points as well as a Map containing links from child to parent,
   * and the depth of the node.
   */
  def breadthFirstTraverse[F[_], A](
      start: A,
      neighbors: A => Queue[A])(
      implicit F: Sync[F]): Iterant[F, (Map[A, A], Int, A)] = {
    def recurse(queue: Queue[(Int, A)], visited: Set[A], paths: Map[A, A]): Iterant[F, (Map[A, A], Int, A)] = {
      if (queue.isEmpty) {
        Iterant.empty
      } else {
        val ((depth, node), tail) = queue.dequeue
        if (visited contains node) {// Could be visited after placed in queue
          recurse(tail, visited, paths)
        } else {
          val newNeighbors = neighbors(node) filterNot {visited contains _}
          val newPaths = paths ++ (newNeighbors map {n => (n, node)})
          val newNeighborsWithDepth = newNeighbors map {n => (depth + 1, n)}
          Iterant.pure((paths, depth, node)) ++ recurse(tail ++ newNeighborsWithDepth, visited + node, newPaths)
        }
      }
    }
    recurse(Queue((0, start)), Set.empty[A], Map.empty[A, A])
  }

  /*
   * Given paths returned from breadthFirstTraverse, return a path
   * from the start of the search to the specified node
   */
  def calculatePath[A](paths: Map[A, A], from: A): List[A] = {
    @scala.annotation.tailrec
    def recurse(from: A, accum: List[A]): List[A] = {
      val next = paths.get(from)
      next match {
        case Some(next) => recurse(next, from :: accum)
        case None       => from :: accum
      }
    }
    recurse(from, List.empty[A])
  }
}
