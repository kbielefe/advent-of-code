package common

object ElfCode {
  def registerAB(f: (Int, Int) => Int)(instruction: Vector[Int], registers: Vector[Int]): Vector[Int] = {
    val a = registers(instruction(1))
    val i = instruction(2)
    val b = if (i >= 0 && i < registers.size) registers(i) else 0
    val c = f(a, b)
    registers.updated(instruction(3), c)
  }

  def immediateA(f: (Int, Int) => Int)(instruction: Vector[Int], registers: Vector[Int]): Vector[Int] = {
    val a = instruction(1)
    val i = instruction(2)
    val b = if (i >= 0 && i < registers.size) registers(i) else 0
    val c = f(a, b)
    registers.updated(instruction(3), c)
  }

  def immediateB(f: (Int, Int) => Int)(instruction: Vector[Int], registers: Vector[Int]): Vector[Int] = {
    val a = registers(instruction(1))
    val b = instruction(2)
    val c = f(a, b)
    registers.updated(instruction(3), c)
  }

  def compare(f: (Int, Int) => Boolean)(a: Int, b: Int): Int = if (f(a, b)) 1 else 0

  val addr = registerAB{_ + _} _
  val addi = immediateB{_ + _} _
  val mulr = registerAB{_ * _} _
  val muli = immediateB{_ * _} _
  val banr = registerAB{_ & _} _
  val bani = immediateB{_ & _} _
  val borr = registerAB{_ | _} _
  val bori = immediateB{_ | _} _
  val setr = registerAB{case (a, _) => a} _
  val seti = immediateA{case (a, _) => a} _
  val gtir = immediateA{compare{_ > _}} _
  val gtri = immediateB{compare{_ > _}} _
  val gtrr = registerAB{compare{_ > _}} _
  val eqir = immediateA{compare{_ == _}} _
  val eqri = immediateB{compare{_ == _}} _
  val eqrr = registerAB{compare{_ == _}} _

  type Instruction = (Vector[Int], Vector[Int]) => Vector[Int]
  val instructions = Vector(eqir, borr, addr, gtri, muli, gtir, mulr, banr, bori, eqri, eqrr, bani, setr, gtrr, addi, seti)

  def execute(program: List[Vector[Int]]): Vector[Int] = {
    program.foldLeft(Vector(0, 0, 0, 0)){case (registers, instruction) => instructions(instruction.head)(instruction, registers)}
  }

  @scala.annotation.tailrec
  def executeWithJumps(ipReg: Int, program: Vector[Vector[Int]], registers: Vector[Int] = Vector(0, 0, 0, 0, 0, 0), ip: Int = 0): Vector[Int] = {
    if (ip >= program.size || ip < 0) {
      registers
    } else {
      val registersWithIp = registers.updated(ipReg, ip)
      val registersAfterExecution = instructions(program(ip).head)(program(ip), registersWithIp)
      val newIp = registersAfterExecution(ipReg) + 1
      executeWithJumps(ipReg, program, registersAfterExecution, newIp)
    }
  }
}
