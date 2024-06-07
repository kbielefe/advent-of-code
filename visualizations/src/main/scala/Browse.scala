package visualizations

import java.awt.Desktop
import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Using

object Browse:
  /** Write html to a temporary file with the given prefix and suffix and open
   *  in the default web browser. */
  def apply(html: String, prefix: String = "aoc-visualization-", suffix: String = ".html"): Unit =
    val file = File.createTempFile(prefix, suffix)
    Using(BufferedWriter(FileWriter(file, true))) { writer =>
      writer.write(html)
    }
    Desktop.getDesktop.browse(file.toURI)
