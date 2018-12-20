package aoc

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
}
