package common

import cats.effect._
import cats.implicits._
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._
import outwatch._
import outwatch.dsl._

object Runner extends TaskApp {
  private val days = List(
    advent2019.Day1
  )

  def run(args: List[String]): Task[ExitCode] = {
    val hello = h1("Hello World")
    OutWatch.renderInto[Task]("#outwatch", hello).as(ExitCode.Success)
  }

  /*
    <label for="year">Year:</label>
    <select id="year"></select>
    <label for="day">Day:</label>
    <select id="day"></select>
    <label for="part">Part:</label>
    <select id="part">
      <option>1</option>
      <option>2</option>
    </select>
    <button id="run">Run</button>
    <input type="text" id="answer" readonly>
    <button id="copy">Copy</button>
    <span id="time"></span>
    <br>
    <textarea id="input"></textarea>
    */

  private def createUi: Unit = {
    val daySelect = document.getElementById("day").asInstanceOf[dom.html.Select]
    val yearSelect = document.getElementById("year").asInstanceOf[dom.html.Select]
    val runButton = document.getElementById("run").asInstanceOf[dom.html.Button]
    val copyButton = document.getElementById("copy").asInstanceOf[dom.html.Button]

    populateSelect("year", days.map(_.year.toString).toSet.toList.sorted)

    yearSelect.onchange = { (e: dom.Event) => setYear; setInput}
    daySelect.onchange  = { (e: dom.Event) => setInput}

    runButton.onclick  = runClicked
    copyButton.onclick = copyClicked

    setYear
    setInput
  }

  private def exceptionString(e: Throwable): String = {
    val os = new java.io.ByteArrayOutputStream()
    val ps = new java.io.PrintStream(os)
    e.printStackTrace(ps)
    ps.flush()
    val result = os.toString
    ps.close()
    os.close()
    result
  }

  private def populateSelect(id: String, values: List[String]): Unit = {
    val element = document.getElementById(id).asInstanceOf[dom.html.Select]
    element.options.length = 0
    values.foreach{value =>
      val option = document.createElement("option")
      option.textContent = value
      element.appendChild(option)
    }
    element.selectedIndex = element.length - 1
  }

  private def setInput: Unit = {
    val daySelect = document.getElementById("day").asInstanceOf[dom.html.Select]
    val yearSelect = document.getElementById("year").asInstanceOf[dom.html.Select]
    val input = document.getElementById("input").asInstanceOf[dom.html.TextArea]

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

  private def setYear: Unit = {
    val partSelect = document.getElementById("part").asInstanceOf[dom.html.Select]
    val yearSelect = document.getElementById("year").asInstanceOf[dom.html.Select]
    val time = document.getElementById("time")
    val visualization = document.getElementById("visualization").asInstanceOf[dom.html.Div]
    populateSelect("day", days.filter(_.year == yearSelect.value.toInt).map(_.day.toString).toList.sorted)
    partSelect.selectedIndex = 0
    time.textContent = ""
    visualization.innerHTML = ""
  }

  private def copyClicked(e: dom.Event): Unit = {
    val answer = document.getElementById("answer").asInstanceOf[dom.html.Input]
    answer.select()
    document.execCommand("copy")
  }

  private def runClicked(e: dom.Event): Unit = {
    val answer = document.getElementById("answer").asInstanceOf[dom.html.Input]
    val input = document.getElementById("input").asInstanceOf[dom.html.TextArea]
    val partSelect = document.getElementById("part").asInstanceOf[dom.html.Select]
    val time = document.getElementById("time")
    val visualization = document.getElementById("visualization").asInstanceOf[dom.html.Div]
    val yearSelect = document.getElementById("year").asInstanceOf[dom.html.Select]
    val daySelect = document.getElementById("day").asInstanceOf[dom.html.Select]

    visualization.innerHTML = ""
    val dayOpt = days.find(day => day.year == yearSelect.value.toInt && day.day == daySelect.value.toInt)
    dayOpt.map{day =>
      val processedInput = day.input(input.value)
      val part = if (partSelect.value == "1") day.part1 _ else day.part2 _
      part(processedInput).timed.foreachL{result =>
        answer.value = result._2.toString
        time.textContent = s"${result._1.toMillis} ms"
        }.onErrorHandle{e =>
          visualization.innerHTML = s"<pre>${exceptionString(e)}</pre>"
        }
          .runToFuture
    }
  }
}
