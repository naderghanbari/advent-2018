package aoc

/**
  * Disclaimer: this works only for my puzzle input.
  * Other puzzle inputs might model a different problem so I did not bother finding
  * the magic number of 10551260L programmatically. One could mathematically prove that this
  * program returns 22157688 at the end (part two) but that needs a few hours more which I don't have
  * at the moment and there is no real value to it either.
  *
  * My Puzzle input has a mini sub-routine embedded in it which factorizes 10551260L in a very crude
  * inefficient O(n2) manner.
  */
object Day19 extends App {

  case class Inst(operation: String, A: Int, B: Int, C: Int) {
    def runOn(cpu: CPU): CPU = operation match {
      case "addr" => cpu storeInto C valueOf (cpu at A) + (cpu at B)
      case "addi" => cpu storeInto C valueOf (cpu at A) + B
      case "mulr" => cpu storeInto C valueOf (cpu at A) * (cpu at B)
      case "muli" => cpu storeInto C valueOf (cpu at A) * B
      case "banr" => cpu storeInto C valueOf (cpu at A) & (cpu at B)
      case "bani" => cpu storeInto C valueOf (cpu at A) & B
      case "borr" => cpu storeInto C valueOf (cpu at A) | (cpu at B)
      case "bori" => cpu storeInto C valueOf (cpu at A) | B
      case "setr" => cpu storeInto C valueOf (cpu at A)
      case "seti" => cpu storeInto C valueOf A
      case "gtir" => cpu storeInto C valueOf (if (A > (cpu at B)) 1 else 0)
      case "gtri" => cpu storeInto C valueOf (if ((cpu at A) > B) 1 else 0)
      case "gtrr" => cpu storeInto C valueOf (if ((cpu at A) > (cpu at B)) 1 else 0)
      case "eqir" => cpu storeInto C valueOf (if (A == (cpu at B)) 1 else 0)
      case "eqri" => cpu storeInto C valueOf (if ((cpu at A) == B) 1 else 0)
      case "eqrr" => cpu storeInto C valueOf (if ((cpu at A) == (cpu at B)) 1 else 0)
    }
  }

  case class CPU(regs: Vector[Int], ipRegister: Int) {
    val ip = this at ipRegister
    def at(r: Int) = regs(r)
    def storeInto(r: Int) = Write(this, r)
    def run(instruction: Inst) = instruction runOn this
    def ++ = this storeInto ipRegister valueOf ip + 1
  }

  case class Write(d: CPU, r: Int) {
    def valueOf(value: Int) = d.copy(regs = d.regs.updated(r, value))
  }

  val IpRegister = "^#ip (\\d+)$".r
  val InstPattern = "^(....)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)$".r

  val (first, rest) = DataSource.linesFromTextFile("day-19-input.txt").toVector.map(_.trim).splitAt(1)

  val ipRegister = first match {
    case Vector(IpRegister(x)) => x.toInt
  }

  val program = rest.map { case InstPattern(op, iA, iB, iC) => Inst(op, iA.toInt, iB.toInt, iC.toInt) }
  val initialState = CPU(Vector(0, 0, 0, 0, 0, 0), ipRegister)

  val Some(haltState) =
    Iterator
      .continually('X)
      .scanLeft((initialState, false)) {
        case ((c, true), _) => c -> true
        case ((c, false), _) if c.ip >= program.size => c -> true
        case ((c, false), _) => (program(c.ip) runOn c).++ -> false
      }
      .collectFirst { case (halted, true) => halted }

  println("Part 1: ", haltState)

  val escaped = OptimizeProgram.run().map(_.toInt)
  val partTwoInit = CPU(escaped, ipRegister)

  val Some(partTwoHalt) =
    Iterator
      .continually('X)
      .scanLeft((partTwoInit, false)) {
        case ((c, true), _) => c -> true
        case ((c, false), _) if c.ip >= program.size => c -> true
        case ((c, false), _) => (program(c.ip) runOn c).++ -> false
      }
      .collectFirst { case (halted, true) => halted }

  println("Part 2: ", partTwoInit)
}

/**
  * Instructions 4, 5, 6, 8, 9, 10, 11 are executed in a loop with 12 being the escape hatch
  * 7 just adds more to R3 and helps with the escape
  *
  * After careful inspection it seems to be factorizing number 10551260 and adding all divisors (twice)
  *
  * The only optimization I did was to search (1, N/a) to find a b such that b*a=N which leads to
  * O(n*lgn) (harmonic series)
  * I also abstracted R5 (boolean flag) and R4 (my IP) away so that the loops are easier to read.
  *
  * R0 = 0
  * R1 = 10551260
  * R2 = 1
  * R3 = 1
  *
  * R5 = R3 * R2
  * R5 = IF (R5 == R1) 1 ELSE 0
  * R4 = R5 + R4 (IF R5 == 1 JUMP to INST7)
  *
  * INST7: R0 = R3 + R0
  *
  * R2++
  * R5 = IF (R2 > R1) 1 ELSE 0
  * R4 = R4 + R5 (IF R5 == 1 JUMP to INST12)
  * REPEAT
  *
  * INST12:
  * R3 ++
  * R5 = IF (R3 > R1) 1 ELSE 0
  * R4 = R4 + R5 (IF R5 == 1 JUMP to INST16)
  * INST16:
  * ESCAPE HATCH
  */
object OptimizeProgram {
  def run() = {
    var R0 = 0L
    var R2 = 1L
    var R3 = 1L

    while (R3 <= 10551260L) { // INST #13 #14 #15 LOOP
      R2 = 1 // INST #2
      while (R2 <= (10551260L / R3) + 2) { // INST ##9 #10 #11 LOOP (R5 and R4 abstracted away)
        if (R3 * R2 == 10551260L) // INST #3 #4 #5 #6 (R5 and R4 abstracted away)
          R0 += R3 // INST #7
        R2 += 1L // INST #8
      }
      R3 += 1L // INST #12
    }
    val R4 = 16 // INST #16 ESCAPE HATCH
    val R5 = 1
    Vector(R0, 10551260L, R2, R3, R4, R5)
  }
}
