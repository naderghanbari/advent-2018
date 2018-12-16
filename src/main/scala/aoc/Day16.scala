package aoc

import scala.annotation.tailrec

object Day16 extends App {

  val StatePattern = "^.*\\[(\\d+), (\\d+), (\\d+), (\\d+)\\]$".r
  val InstPattern = "^(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)$".r

  val samples =
    DataSource
      .linesFromTextFile("day-16-1-input.txt")
      .grouped(4)
      .map {
        case Seq(
        StatePattern(bA, bB, bC, bD),
        InstPattern(iO, iA, iB, iC),
        StatePattern(aA, aB, aC, aD), _) =>
          val before = Device(Vector(bA.toInt, bB.toInt, bC.toInt, bD.toInt))
          val after = Device(Vector(aA.toInt, aB.toInt, aC.toInt, aD.toInt))
          Sample(iO.toInt, iA.toInt, iB.toInt, iC.toInt, before, after)
      }
      .toVector

  case class Instruction(operation: String, A: Int, B: Int, C: Int) extends (Device => Device) {
    def apply(d: Device): Device = operation match {
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
  }

  case class Write(d: Device, r: Int) {
    def valueOf(value: Int) = d.copy(registers = d.registers.updated(r, value))
  }

  case class Sample(opcode: Int, A: Int, B: Int, C: Int, before: Device, after: Device) {
    def behavesLike: List[String] =
      Instructions
        .map(inst => Instruction(inst, A, B, C))
        .collect { case inst if inst(before) == after => inst.operation }
        .toList
  }


  val candidates = samples.map(_.behavesLike)
  val count = candidates.count(_.size > 2)

  println(s"Part 1: $count")

  val constraints: List[(Int, List[String])] = (samples.map(_.opcode), candidates).zipped.toList.sortBy(_._2.size)

  @tailrec def naiveCSP(cs: List[(Int, List[String])], soFar: Map[Int, String]): Map[Int, String] =
    soFar.keySet.size match {
      case 16 => soFar
      case _ => cs match {
        case (opcode, inst :: Nil) :: tail =>
          val remainingConstraints = tail.collect { case (o, choices) if o != opcode => (o, choices.filterNot(_ == inst)) }
          naiveCSP(remainingConstraints.sortBy(_._2.size), soFar + (opcode -> inst))
        case Nil => throw new IllegalStateException("Should not happen")
      }
    }

  val instMap = naiveCSP(constraints, Map.empty[Int, String])

  val program =
    DataSource
      .linesFromTextFile("day-16-2-input.txt")
      .map {
        case InstPattern(iO, iA, iB, iC) => Instruction(instMap(iO.toInt), iA.toInt, iB.toInt, iC.toInt)
      }
      .toVector

  val initialState = Device(Vector(0, 0, 0, 0))

  val endResult = program.foldLeft(initialState) { (device, instruction) => instruction apply device }

  println(s"Part 2: ${endResult at 0}")

}
