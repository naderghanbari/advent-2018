package aoc

import scala.math.abs

object Day06 extends App {

  val lines = DataSource.linesFromTextFile("day-6-input.txt").toList
  val points = lines.map(_.split(",").map(_.trim.toInt).toList).map { case List(x, y) => Point(x, y) }
  val (xMin, xMax) = (points.map(_.x).min, points.map(_.x).max)
  val (yMin, yMax) = (points.map(_.y).min, points.map(_.y).max)
  val radius = Seq(xMax - xMin, yMax - yMin).max

  def distTo(p: Point)(q: Point) = abs(p.x - q.x) + abs(p.y - q.y)
  def totalDistance(q: Point) = points.map(distTo(q)).sum

  def closest(q: Point): Seq[Point] = points.groupBy(distTo(q)).minBy(_._1)._2 match {
    case singleton@Seq(_) => singleton
    case _ => Nil
  }

  def areaGrid(margin: Int) = for {
    i <- (xMin - margin to xMax + margin).par
    j <- (yMin - margin to yMax + margin).view
    c <- closest(Point(i, j))
  } yield c

  val original = areaGrid(radius).groupBy(identity).mapValues(_.length)
  val witness = areaGrid(radius + 5).groupBy(identity).mapValues(_.length)
  val maxArea = original.collect { case (p, area) if witness.get(p).contains(area) => area }.max

  println(s"Part 1: $maxArea")

  val acceptable = 10000
  val grid = for {
    i <- (xMin - 100 to xMax + 100).view
    j <- (yMin - 100 to yMax + 100).view
    p = Point(i, j)
    c = totalDistance(p) if c < acceptable
  } yield 1

  println(s"Part 2: ${grid.sum}")

}
