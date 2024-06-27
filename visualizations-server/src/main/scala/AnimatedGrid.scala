package visualizations

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom
import org.scalajs.dom.html.Element

@JSExportTopLevel("AnimatedGrid")
object AnimatedGrid:
  @JSExport
  def animate(jsInitial: js.Dictionary[String], jsSteps: js.Array[js.Dictionary[js.Dictionary[String]]]): Unit =
    val initial = jsInitial.map(parseGrid.tupled).toMap
    val steps = jsSteps.map(parseSteps).toList

    val minRow = initial.keySet.map(_._1).min
    val maxRow = initial.keySet.map(_._1).max
    val minCol = initial.keySet.map(_._2).min
    val maxCol = initial.keySet.map(_._2).max

    val grid = (minRow to maxRow).map{row =>
      (minCol to maxCol).map{col =>
        s"<td id='$row,$col' style='height: 1em; width: 1em'>${initial(row -> col)}</td>"
      }.mkString
    }.mkString("<tr>", "</tr>\n<tr>", "</tr>\n")

    dom.document.querySelector("#grid").innerHTML = s"<table>$grid</table>"
    animateSteps(steps)
  end animate

  private def animateSteps(steps: List[Step]): Unit =
    if !steps.isEmpty then
      steps.head.run
      js.timers.setTimeout(1000/30)(animateSteps(steps.tail))

  private type Pos = (Int, Int)
  private def parseGrid(key: String, value: String): (Pos, Char) =
    parsePos(key) -> value.head

  private def parsePos(key: String): Pos =
    val regex = """\((\d+),(\d+)\)""".r
    key match
      case regex(row, col) => row.toInt -> col.toInt

  private case class Step(chars: Map[Pos, Char], colors: Map[Pos, String]):
    def run: Unit =
      chars.foreach{case ((row, col), char) =>
        dom.document.getElementById(s"$row,$col").innerHTML = char.toString
      }
      colors.foreach{case ((row, col), color) =>
        dom.document.getElementById(s"$row,$col").asInstanceOf[Element].style.color = color
      }

  private def parseSteps(jsStep: js.Dictionary[js.Dictionary[String]]): Step =
    val chars = jsStep("chars").map{case (key, value) => parsePos(key) -> value.head}.toMap
    val colors = jsStep("colors").map{case (key, value) => parsePos(key) -> value}.toMap
    Step(chars, colors)
