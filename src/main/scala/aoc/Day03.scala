package aoc

/** Using `par` is not exactly dumb here, it's applied on a
  * cartesian product of two ranges so it provides a
  * quasi-divide-and-conquer solution.
  * This is way far from an optimum algorithm though.
  */
object Day03 extends App {

  val data = DataSource.linesFromTextFile("day-3-input.txt")

  val claims = data.map(Claim.parse).toSeq

  val Some(winner) = claims.find(claim => claims.count(!_.disjoint(claim)) == 1)
  println(s"Part B: ${winner.id}")

  val rects = claims.map(_.rect)
  val range = (1 to 1000).view.par
  val hotSpotCount = for {
    x <- range
    y <- range
    p = Point(x, y)
    if rects.dropWhile(!_.covers(p)).drop(1).dropWhile(!_.covers(p)).nonEmpty
  } yield 1

  println(s"Part A: ${hotSpotCount.sum}")

}

case class Point(x: Int, y: Int)

case class Rect(nw: Point, se: Point) {
  def covers(p: Point): Boolean = (p.x <= se.x) && (p.x >= nw.x) && (p.y <= se.y) && (p.y >= nw.y)
}

case class Claim(id: String, left: Int, top: Int, w: Int, h: Int) {
  val rect = Rect(Point(top + 1, left + 1), Point(top + h, left + w))

  def disjoint(that: Claim) =
    (that.rect.nw.x > rect.se.x) || (that.rect.nw.y > rect.se.y) ||
      (rect.nw.x > that.rect.se.x) || (rect.nw.y > that.rect.se.y)
}

object Claim {
  private val ClaimPattern = "^#(\\d+)\\s+@\\s+(\\d+),(\\d+):\\s*(\\d+)x(\\d+)$".r

  def parse(s: String): Claim = s match {
    case ClaimPattern(id, t, l, w, h) => Claim(id, t.toInt, l.toInt, w.toInt, h.toInt)
  }
}
