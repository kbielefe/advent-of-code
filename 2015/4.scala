import java.security.MessageDigest

val input = "ckczppom"

def keys = Iterator.from(1) map {input + _}

def md5s = keys map {key =>
  val md5 = MessageDigest.getInstance("MD5")
  md5.update(key.getBytes)
  md5.digest
}

val answer1 = (keys zip md5s).find(x => x._2(0) == 0 && x._2(1) == 0 && (x._2(2) & 0xf0) == 0).get._1
println(answer1)

val answer2 = (keys zip md5s).find(x => x._2(0) == 0 && x._2(1) == 0 && x._2(2) == 0).get._1
println(answer2)