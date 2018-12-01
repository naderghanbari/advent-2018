package aoc

import scala.io.Source

object DataSource {

  def linesFromTextFile(name: String): Iterator[String] =
    Source.fromResource(name).getLines()

  def intLinesFromTextFile(name: String): Iterator[Int] =
    linesFromTextFile(name).map(_.toInt)

}
