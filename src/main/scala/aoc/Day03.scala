package aoc

object Day03 extends App {
  lazy val data = DataSource.linesFromTextFile("day-3-input.txt").toStream
  lazy val rects = data.map(Claim.parse).map(_.toRect)

  val chracteristics = for {
    x <- (1 to 1000).view
    y <- (1 to 1000).view
    p = Point(x, y)
    c = if (rects.dropWhile(!_.covers(p)).drop(1).dropWhile(!_.covers(p)).nonEmpty) 1 else 0
  } yield c

  println(chracteristics.sum)
}

case class Point(x: Int, y: Int)
case class Rect(nw: Point, se: Point) {
  def covers(p: Point): Boolean = (p.x <= se.x) && (p.x >= nw.x) && (p.y <= se.y) && (p.y >= nw.y)
}
case class Claim(id: String, fromLeft: Int, fromTop: Int, width: Int, height: Int) {
  def toRect = Rect(
    Point(fromTop + 1, fromLeft + 1),
    Point(fromTop + height, fromLeft + width)
  )
}

object Claim {
  private val ClaimPattern = "^#(\\d)+\\s+@\\s+(\\d+),(\\d+):\\s*(\\d+)x(\\d+)$".r

  def parse(s: String): Claim = s match {
    case ClaimPattern(id, t, l, w, h) => Claim(id, t.toInt, l.toInt, w.toInt, h.toInt)
  }
}

