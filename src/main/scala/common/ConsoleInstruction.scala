package common

sealed trait ConsoleInstruction
case class Nop(unused: Int) extends ConsoleInstruction
case class Jmp(dest: Int)   extends ConsoleInstruction
case class Acc(inc: Int)    extends ConsoleInstruction

