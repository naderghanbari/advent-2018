package aoc

import aoc.Day19.{IpRegister, InstPattern, CPU, Inst}

object Day21 extends App {

  val (first, rest) = DataSource.linesFromTextFile("day-21-input.txt").toVector.map(_.trim).splitAt(1)
  val Vector(IpRegister(ipRegister)) = first
  val program = rest.map { case InstPattern(op, iA, iB, iC) => Inst(op, iA.toInt, iB.toInt, iC.toInt) }

  val r5Native = Stream.continually('X)
    .scanLeft((CPU(Vector(0, 0, 0, 0, 0, 0), ipRegister.toInt), false, 0)) {
      case ((c, true, count), _) => (c, true, count)
      case ((c, false, count), _) if c.ip >= program.size => (c, true, count)
      case ((c, false, count), _) => ((program(c.ip) runOn c).++, false, count + 1)
    }
    .collect { case (reached, _, _) if reached.ip == 28 => reached at 5 }

  println(s"Part 1: ${r5Native.head}")

  val r5Optimized = Stream.continually('X).scanLeft(0)((r5, _) => Routine.iter(r5))
  val Some((_, index)) = Day18.firstDuplicateWithIndex(r5Optimized)
  println(s"Part 2: ${r5Optimized(index - 1)}")

}

object Routine extends App {
  def iter(r5: Int): Int = {
    var R3 = r5 | 65536
    var R5 = 7586220
    // ## SECOND LOOP ## // #8
    var loop = true
    while (loop) {
      R5 += R3 & 255 // #9
      R5 = ((R5 & 16777215) * 65899) & 16777215 // // #10 #11 #12
      loop = 256 <= R3
      if (loop) {
        var R1 = 0 // #17
        do R1 += 1 while ((256 * (R1 + 1)) <= R3) // ## THIRD LOOP (could be optimized to return the value right away)
        R3 = R1 // #26
      }
    }
    R5
  }
}
