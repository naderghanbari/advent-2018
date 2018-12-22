package aoc

import aoc.Day19.{IpRegister, InstPattern, CPU, Inst}

object Day21 extends App {

  val (first, rest) = DataSource.linesFromTextFile("day-21-input.txt").toVector.map(_.trim).splitAt(1)
  val Vector(IpRegister(ipRegister)) = first
  val program = rest.map { case InstPattern(op, iA, iB, iC) => Inst(op, iA.toInt, iB.toInt, iC.toInt) }

  val initialState = CPU(Vector(0, 0, 0, 0, 0, 0), ipRegister.toInt)

  val Some(finalState) =
    Iterator
      .continually('X)
      .scanLeft((initialState, false, 0)) {
        case ((c, true, count), _) => (c, true, count)
        case ((c, false, count), _) if c.ip >= program.size => (c, true, count)
        case ((c, false, count), _) =>
          val inst = program(c.ip)
          val after = inst runOn c
          (after.++, false, count + 1)
      }
      .collectFirst { case (reached, _, _) if reached.ip == 28 => reached }

  println("Part 1: " + (finalState at 5))
}

/** Instructions transpiled and optimized */
object Routine extends App {
  var R0 = 0
  var R1 = 1
  var R3 = 65536
  var R4 = 0
  var R5 = 7586220

  while (true) {
    R3 = R5 | 65536
    R5 = 7586220
    // ## SECOND LOOP ## // #8
    var loop = true
    while (loop) {
      R1 = R3 & 255 // #8
      R5 += R1 // #9
      R5 = R5 & 16777215 // #10
      R5 = R5 * 65899 // #11
      R5 = R5 & 16777215 // #12
      R1 = if (256 > R3) 1 else 0 // #13
      loop = R1 == 0
      if (loop) {
        R1 = 0 // #17
        do R1 += 1 while ((256 * (R1 + 1)) <= R3) // ## THIRD LOOP ##
        R3 = R1 // #26
      }
    }
    println(R5)
    if (R5 == R0) { // #29
      println("HALTING")
      println(R0, R1, 0, R3, R4, R5)
      throw new IllegalStateException("HALTED")
    }
  } // #30

  println(R0, R1, 0, R3, R4, R5)

}