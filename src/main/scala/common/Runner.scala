package common

import cats.effect._
import cats.implicits._
import monix.eval._
import monix.reactive.Observable
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._

abstract class Day[I, A, B](val year: Int, val day: Int) {
  def input(string: String): I
  def part1(input: I): Task[A]
  def part2(input: I): Task[B]
}

abstract class IntsDay[A, B](year: Int, day: Int) extends Day[Observable[Int], A, B](year, day) {
  override def input(string: String): Observable[Int] =
    Observable.fromIterator(Task(string.linesIterator)).filter(!_.isEmpty).map(_.toInt)
}

package advent2019 {
  object Day1 extends IntsDay[Int, Int](2019, 1) {
    private def compound(in: Int): Task[Int] =
      Observable.unfold(in){mass =>
        val fuel = mass / 3 - 2
        if (fuel > 0) Some(fuel -> fuel) else None
      }.sumL

    override def part1(input: Observable[Int]): Task[Int] = input.map(_ / 3 - 2).sumL
    override def part2(input: Observable[Int]): Task[Int] = input.mapEval(compound).sumL
  }
}

object Runner extends TaskApp {
  private val days = List(
    advent2019.Day1
  )

  def run(args: List[String]): Task[ExitCode] =
    Task(createYear).as(ExitCode.Success)

  private def createYear: Unit = {
    val daySelect = document.createElement("select").asInstanceOf[dom.html.Select]
    val yearSelect = document.createElement("select").asInstanceOf[dom.html.Select]
    val partSelect = document.createElement("select").asInstanceOf[dom.html.Select]
    val runButton = document.createElement("button").asInstanceOf[dom.html.Button]
    val copyButton = document.createElement("button").asInstanceOf[dom.html.Button]
    val input = document.createElement("textarea").asInstanceOf[dom.html.TextArea]
    val answer = document.createElement("input").asInstanceOf[dom.html.Input]
    val visualization = document.createElement("div").asInstanceOf[dom.html.Div]
    val time = document.createTextNode("")
    input.style.cssFloat = "left"
    input.style.width = "200px"
    input.style.height = "350px"
    answer.setAttribute("type", "text")
    val years = days.map(_.year).toSet.toList.sorted
    years.foreach{year =>
      val option = document.createElement("option")
      option.textContent = year.toString
      yearSelect.appendChild(option)
    }
    def setYear: Unit = {
      daySelect.selectedIndex = -1
      daySelect.options.length = 0
      val newDays = days.filter(_.year == yearSelect.value.toInt).map(_.day).toList.sorted
      newDays.foreach{day =>
        val option = document.createElement("option")
        option.textContent = day.toString
        daySelect.appendChild(option)
      }
      daySelect.selectedIndex = daySelect.length - 1
      partSelect.selectedIndex = 0
      time.textContent = ""
      visualization.innerHTML = ""
    }
    def setInput: Unit = {
      val xhr = new dom.XMLHttpRequest()
      xhr.open("GET", s"input/${yearSelect.value}/${daySelect.value}.txt")
      xhr.onload = { (e: dom.Event) =>
        if (xhr.status == 200) {
          input.textContent = xhr.responseText
        } else {
          input.textContent = s"Not available"
        }
      }
      xhr.send()
    }
    yearSelect.onchange = { (e: dom.Event) => setYear; setInput}
    daySelect.onchange = { (e: dom.Event) => setInput}
    yearSelect.selectedIndex = yearSelect.length - 1
    List(1, 2).foreach{part =>
      val option = document.createElement("option")
      option.textContent = s"$part"
      partSelect.appendChild(option)
    }
    runButton.textContent = "Run"
    runButton.onclick = { (e: dom.Event) =>
      visualization.innerHTML = ""
      val day = days.find(day => day.year == yearSelect.value.toInt && day.day == daySelect.value.toInt)
      val processedInput = day.get.input(input.value)
      val part = if (partSelect.value == "1") day.get.part1 _ else day.get.part2 _
      part(processedInput).timed.foreachL{result =>
        answer.value = result._2.toString
        time.textContent = s"${result._1.toMillis} ms"
      }.onErrorHandle{e =>
        val os = new java.io.ByteArrayOutputStream()
        val ps = new java.io.PrintStream(os)
        e.printStackTrace(ps)
        ps.flush()
        visualization.innerHTML = s"<pre>${os.toString}</pre>"
        ps.close()
        os.close()
      }
      .runToFuture
    }
    copyButton.textContent = "Copy"
    copyButton.onclick = { (e: dom.Event) =>
      answer.select()
      document.execCommand("copy")
    }
    setYear
    setInput
    document.body.appendChild(document.createTextNode(" Year: "))
    document.body.appendChild(yearSelect)
    document.body.appendChild(document.createTextNode(" Day: "))
    document.body.appendChild(daySelect)
    document.body.appendChild(document.createTextNode(" Part: "))
    document.body.appendChild(partSelect)
    document.body.appendChild(document.createTextNode(" "))
    document.body.appendChild(runButton)
    document.body.appendChild(document.createTextNode(" "))
    document.body.appendChild(answer)
    document.body.appendChild(document.createTextNode(" "))
    document.body.appendChild(copyButton)
    document.body.appendChild(document.createTextNode(" "))
    document.body.appendChild(time)
    document.body.appendChild(document.createElement("br"))
    document.body.appendChild(input)
    document.body.appendChild(visualization)
  }
}
