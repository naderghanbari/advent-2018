package aoc

/**
  * // Use `initial` in lieu of `fastForwarded` if fast forwarding 10000 seconds is a far stretch.
  * // A height diff (for the first few iterations) can be used to infer a good heuristic for
  * // the fast forward (how much the cluster shrinks in radius and height in each iteration)
  */
object Day10 extends App {

  val lines = DataSource.linesFromTextFile("day-10-input.txt").toList
  val initial = Sky(lines.sorted.map(Sky.parse), time = 0)

  val fastForwarded = initial advance 10000

  Iterator
    .continually(1)
    .scanLeft(fastForwarded)(_ advance _)
    .flatMap(_.heuristicMessage)
    .take(1)
    .foreach(println)

}

case class Sky(points: List[SkyPoint], time: Int) {

  def advance(forward: Int) = Sky(points.map(_.moved(forward)), time + forward)

  def heuristicMessage: Option[String] = {
    val ys = points.map(_.y)
    if (ys.max < ys.min + 12) Some(message(ys)) else None
  }

  def message(ys: List[Int]) = {
    val horizontal = points.minBy(_.x).x to points.maxBy(_.x).x
    val byY = points.groupBy(_.y).withDefaultValue(Seq.empty)
    val skyLines = (ys.min to ys.max).view.map { y =>
      val charOf = byY(y).map(_.x).groupBy(identity).mapValues(_ => '#').withDefaultValue(' ')
      horizontal.map(charOf).mkString("")
    }
    s"${skyLines.mkString("\n")}\nWaited $time seconds!"
  }

}

object Sky {
  val Pattern = "^position=<(.+),(.+)> velocity=<(.+), (.+)>$".r

  def parse(s: String) = s match {
    case Pattern(x, y, vx, vy) => SkyPoint(x.trim.toInt, y.trim.toInt, vx.trim.toInt, vy.trim.toInt)
  }

}

case class SkyPoint(x: Int, y: Int, vx: Int, vy: Int) {
  def moved(forward: Int) = SkyPoint(x + forward * vx, y + forward * vy, vx, vy)
}
