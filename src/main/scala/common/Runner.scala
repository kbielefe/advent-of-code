package common

import cats.effect._
import cats.implicits._
import monix.eval._
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._

class Day(val year: Int, val day: Int) {
}

object Runner extends TaskApp {
  private val days = List(
    new Day(2019, 1),
    new Day(2019, 2),
    new Day(2020, 1)
  )

  def run(args: List[String]): Task[ExitCode] =
    Task(createYear).as(ExitCode.Success)

  private def createYear: Unit = {
    val daySelect = document.createElement("select").asInstanceOf[dom.html.Select]
    val yearSelect = document.createElement("select").asInstanceOf[dom.html.Select]
    val partSelect = document.createElement("select").asInstanceOf[dom.html.Select]
    val runButton = document.createElement("button").asInstanceOf[dom.html.Button]
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
    }
    yearSelect.onchange = { (e: dom.Event) => setYear}
    yearSelect.selectedIndex = yearSelect.length - 1
    List(1, 2).foreach{part =>
      val option = document.createElement("option")
      option.textContent = s"$part"
      partSelect.appendChild(option)
    }
    runButton.textContent = "Run"
    runButton.onclick = { (e: dom.Event) =>
      println(s"Running ${yearSelect.value} ${daySelect.value} ${partSelect.value}")
    }
    setYear
    document.body.appendChild(document.createTextNode(" Year: "))
    document.body.appendChild(yearSelect)
    document.body.appendChild(document.createTextNode(" Day: "))
    document.body.appendChild(daySelect)
    document.body.appendChild(document.createTextNode(" Part: "))
    document.body.appendChild(partSelect)
    document.body.appendChild(document.createTextNode(" "))
    document.body.appendChild(runButton)
  }

  private def inputLines(year: Int, day: Int): Observable[String] =
    Observable.fromIterator(input(year, day).map(_.linesIterator))

  private def input(year: Int, day: Int): Task[String] = Task.async{callback =>
    val xhr = new dom.XMLHttpRequest()
    xhr.open("GET", s"input/$year/$day.txt")
    xhr.onload = { (e: dom.Event) =>
      if (xhr.status == 200) {
        callback.onSuccess(xhr.responseText)
      } else {
        callback.onError(new Exception(s"Received status ${xhr.status} retrieving input for $year/$day"))
      }
    }
    xhr.send()
  }
}
