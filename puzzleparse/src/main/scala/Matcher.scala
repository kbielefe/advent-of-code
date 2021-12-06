package puzzleparse
import scala.util.matching.Regex

trait Matcher[A]:
  def regex: Regex
  def extract(text: String): A
