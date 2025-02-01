package advent2015
import common.Day
import scala.io.Source
import java.security.MessageDigest

class Day4(source: Source) extends Day {
  val input = source.mkString("")

  def keys = Iterator.from(1) map {input + _}

  def md5s = keys map {key =>
    val md5 = MessageDigest.getInstance("MD5")
    md5.update(key.getBytes)
    md5.digest
  }

  override def answer1 = (keys zip md5s).find(x => x._2(0) == 0 && x._2(1) == 0 && (x._2(2) & 0xf0) == 0).get._1.toString
  override def answer2 = (keys zip md5s).find(x => x._2(0) == 0 && x._2(1) == 0 && x._2(2) == 0).get._1.toString
}
