import scala.io.Source
import scala.util.Try

val input = Source.fromFile("input23.txt").getLines.map(_ split " ").toVector

class Assembly[S](
    instructions: Vector[Array[String]],
    regUpdate:    Map[String, (Long, Long) => Long],
    pcUpdate:     Map[String, (Long, Long) => Long],
    stateUpdate:  Map[String, (Long, Long, S) => S]) {

  def getArg(registers: Map[String, Long])(arg: String): Long =
    Try(arg.toLong).getOrElse(registers.getOrElse(arg, 0))

  def execute(initialState: S, initialRegs: Map[String, Long] = Map.empty[String, Long]) =
      Iterator.iterate((initialRegs, 0, initialState)){case (regs, pc, state) =>
    def reg = getArg(regs) _
    val Array(instruction, arg1, arg2) = instructions(pc).padTo(3, "")
    def incPc(x: Long, y: Long): Long = 1
    val newPc = pc + pcUpdate.getOrElse(instruction, incPc _)(reg(arg1), reg(arg2)).toInt
    val newRegs = if (regUpdate contains instruction)
        regs + (arg1 -> regUpdate(instruction)(reg(arg1), reg(arg2)))
      else
        regs
    def keepState(x: Long, y: Long, s: S): S = s
    val newState = stateUpdate.getOrElse(instruction, keepState _)(reg(arg1), reg(arg2), state)
    (newRegs, newPc, newState)
  }
}

val regUpdate = Map[String, (Long, Long) => Long](
  "set" -> {case (_, arg2) => arg2},
  "sub" -> {_ - _},
  "mul" -> {_ * _},
)
val pcUpdate = Map[String, (Long, Long) => Long](
  "jnz" -> {case (arg1, arg2) => if (arg1 != 0) arg2 else 1}
)
val stateUpdate = Map[String, (Long, Long, Int) => Int](
  "mul" -> {case (_, _, count) => count + 1}
)
val assembly = new Assembly(input, regUpdate, pcUpdate, stateUpdate)

val answer1 = assembly.execute(0).dropWhile{x => x._2 >= 0 && x._2 < input.size}.map(_._3).next
println(answer1)
