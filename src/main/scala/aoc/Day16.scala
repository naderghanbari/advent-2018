package aoc

import scala.annotation.tailrec

object Day16 extends App {

  val Snap = "^.*\\[(\\d+), (\\d+), (\\d+), (\\d+)\\]$".r
  val Inst = "^(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)$".r

  val samples =
    DataSource
      .linesFromTextFile("day-16-1-input.txt")
      .grouped(4)
      .map(_.take(3))
      .map {
        case Seq(Snap(bA, bB, bC, bD), Inst(iO, iA, iB, iC), Snap(aA, aB, aC, aD)) =>
          val before = Device(Vector(bA.toInt, bB.toInt, bC.toInt, bD.toInt))
          val after = Device(Vector(aA.toInt, aB.toInt, aC.toInt, aD.toInt))
          Sample(iO.toInt, iA.toInt, iB.toInt, iC.toInt, before, after)
      }
      .toVector

  case class Instruction(operation: String, A: Int, B: Int, C: Int) {
    def runOn(d: Device): Device = operation match {
      case "addr" => d storeInto C valueOf (d at A) + (d at B)
      case "addi" => d storeInto C valueOf (d at A) + B
      case "mulr" => d storeInto C valueOf (d at A) * (d at B)
      case "muli" => d storeInto C valueOf (d at A) * B
      case "banr" => d storeInto C valueOf (d at A) & (d at B)
      case "bani" => d storeInto C valueOf (d at A) & B
      case "borr" => d storeInto C valueOf (d at A) | (d at B)
      case "bori" => d storeInto C valueOf (d at A) | B
      case "setr" => d storeInto C valueOf (d at A)
      case "seti" => d storeInto C valueOf A
      case "gtir" => d storeInto C valueOf (if (A > (d at B)) 1 else 0)
      case "gtri" => d storeInto C valueOf (if ((d at A) > B) 1 else 0)
      case "gtrr" => d storeInto C valueOf (if ((d at A) > (d at B)) 1 else 0)
      case "eqir" => d storeInto C valueOf (if (A == (d at B)) 1 else 0)
      case "eqri" => d storeInto C valueOf (if ((d at A) == B) 1 else 0)
      case "eqrr" => d storeInto C valueOf (if ((d at A) == (d at B)) 1 else 0)
    }
  }

  val Instructions = Vector(
    "addr", "addi",
    "mulr", "muli",
    "banr", "bani",
    "borr", "bori",
    "setr", "seti",
    "gtir", "gtri", "gtrr",
    "eqir", "eqri", "eqrr"
  )

  case class Device(registers: Vector[Int]) {
    require(registers.size == 4, "This device has only 4 registers!")
    def at(r: Int) = registers(r)
    def storeInto(r: Int) = Write(this, r)
    def run(instruction: Instruction) = instruction runOn this
  }

  case class Write(d: Device, r: Int) {
    def valueOf(value: Int) = d.copy(registers = d.registers.updated(r, value))
  }

  case class Sample(opcode: Int, A: Int, B: Int, C: Int, before: Device, after: Device) {
    def behavesLike: List[String] =
      Instructions
        .map(inst => Instruction(inst, A, B, C))
        .collect { case inst if (before run inst) == after => inst.operation }
        .toList
  }

  val candidates = samples.map(_.behavesLike)
  val count = candidates.count(_.size > 2)

  println(s"Part 1: $count")

  val constraints = (samples.map(_.opcode), candidates).zipped.toList.sortBy(_._2.size)

  @tailrec def naiveCSP(cs: List[(Int, List[String])], soFar: Map[Int, String]): Map[Int, String] = cs match {
    case Nil => soFar
    case (opcode, inst :: Nil) :: tail =>
      val remaining = tail.collect { case (o, choices) if o != opcode => (o, choices.filterNot(_ == inst)) }
      val remainingSorted = remaining.sortBy(_._2.size)
      naiveCSP(remainingSorted, soFar + (opcode -> inst))
  }

  val instructionFromOpcode = naiveCSP(constraints, Map.empty[Int, String])
  val initialState = Device(Vector(0, 0, 0, 0))
  val program =
    DataSource
      .linesFromTextFile("day-16-2-input.txt")
      .map { case Inst(iO, iA, iB, iC) =>
        Instruction(instructionFromOpcode(iO.toInt), iA.toInt, iB.toInt, iC.toInt)
      }

  val endResult = program.foldLeft(initialState)(_ run _)

  println(s"Part 2: ${endResult at 0}")

}
