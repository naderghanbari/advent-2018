package aoc

import scala.annotation.tailrec
import scala.math.abs

/** Most things are Streams so part 1 could work on a really long input without
  * consuming a lot of memory.
  *
  * Part 2:
  * - we can work on the already reduced polymer (no need to start over)
  * - `.par` on characters sill speed up counting variances
  */
object Day05 extends App {

  @tailrec def twofold(past: Stream[Char], future: Stream[Char], erase: Option[Char]): Stream[Char] =
    (past, future) match {
      case (a #:: pt, b #:: ft) if abs(a - b) == 32 => twofold(pt, ft, erase)
      case (_, b #:: ft) if erase.contains(b.toLower) => twofold(past, ft, erase)
      case (_, b #:: ft) => twofold(b #:: past, ft, erase)
      case (_, Stream.Empty) => past.reverse
    }

  val polymer = DataSource.charsFromTextFile("day-5-input.txt").toStream.dropRight(1)
  val reduced = twofold(Stream.Empty, polymer, erase = None)

  println(s"Part A: ${reduced.length}")

  val chars = reduced.groupBy(identity).keys.par
  val min = chars.map(c => twofold(Stream.Empty, reduced, Some(c.toLower)).length).min

  println(s"Part 2: $min")

}
