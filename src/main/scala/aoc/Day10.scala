package aoc

object Day10 extends App {

  val lines = DataSource.linesFromTextFile("day-10-input.txt").toList
  val skyPoints = lines.sorted.map(Sky.parse)

  def skyMap(points: Seq[SkyPoint]) = {
    val horizontal = points.minBy(_.x).x to points.maxBy(_.x).x
    if (horizontal.size < 200) {
      val vertical = points.minBy(_.y).y to points.maxBy(_.y).y
      val byY = points.groupBy(_.y).withDefaultValue(Seq.empty)
      val skyLines =
        vertical
          .map { y =>
            val charOf = byY(y).map(_.x).groupBy(identity).mapValues(_ => '*').withDefaultValue('.')
            horizontal.map(charOf).mkString("")
          }
      Some(skyLines.mkString("\n"), points.head.sec)
    } else {
      Option.empty[String]
    }
  }

  Iterator
    .continually(1)
    .scanLeft(skyPoints) {
      case (sky, _) => sky.map(_.moved)
    }
    .flatMap(skyMap)
    .slice(10, 20)
    .foreach { s =>
      println(s)
      println("///////////////////////////////////////////////////////")
    }

  println(skyMap(skyPoints))

}

case class SkyPoint(x: Int, y: Int, vx: Int, vy: Int, sec: Int) {
  def moved = SkyPoint(x + vx, y + vy, vx, vy, sec + 1)
}

object Sky {
  val Pattern = "^position=<(.+),(.+)> velocity=<(.+), (.+)>$".r

  def parse(s: String) = s match {
    case Pattern(x, y, vx, vy) => SkyPoint(x.trim.toInt, y.trim.toInt, vx.trim.toInt, vy.trim.toInt, 0)
  }

}

