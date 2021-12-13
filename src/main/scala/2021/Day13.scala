package advent2021
import puzzleparse.Pos

object Day13:
  def part1(input: String): Int =
    val dots = getDots(input)
    val folds = getFolds(input)
    folds.head.fold(dots).size

  def part2(input: String): String =
    val dots = getDots(input)
    val folds = getFolds(input)
    display(folds.foldLeft(dots)((dots, fold) => fold.fold(dots)))

  private def display(dots: Set[Pos]): String =
    (dots.map(_.row).min to dots.map(_.row).max).map{row =>
      (dots.map(_.col).min to dots.map(_.col).max).map{col =>
        if dots.contains(Pos(row, col)) then '█' else ' '
      }.mkString
    }.mkString("\n")

  private def getDots(input: String): Set[Pos] =
    input.split("\n\n")(0).split("\n").toSet.map(_.split(",").map(_.toInt)).map{case Array(x, y) => Pos(y, x)}

  private def getFolds(input: String): List[Fold] =
    input.split("\n\n")(1).split("\n").toList.map(Fold(_))

  sealed trait Fold:
    def fold(dots: Set[Pos]): Set[Pos]

  case class Vertical(x: Int) extends Fold:
    override def fold(dots: Set[Pos]): Set[Pos] = dots.map{dot =>
      if dot.col > x then Pos(dot.row, 2 * x - dot.col) else dot
    }

  case class Horizontal(y: Int) extends Fold:
    override def fold(dots: Set[Pos]): Set[Pos] = dots.map{dot =>
      if dot.row > y then Pos(2 * y - dot.row, dot.col) else dot
    }

  object Fold:
    val regex = "(.)=(\\d+)".r.unanchored
    def apply(line: String): Fold = line match
      case regex("x", x) => Vertical(x.toInt)
      case regex("y", y) => Horizontal(y.toInt)
