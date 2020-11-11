package common

import cats.effect._
import cats.implicits._
import monix.eval._
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.document

object Runner extends TaskApp {
  def run(args: List[String]): Task[ExitCode] =
    Task(createYear).as(ExitCode.Success)

  private def createYear: Unit = {
    val year = document.createElement("select")
    val option = document.createElement("option")
    option.textContent = "2019"
    year.appendChild(option)
    document.body.appendChild(year)
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
