package aoc

import scala.io.Source

object DataSource {

  def linesFromTextFile(name: String): Iterator[String] =
    Source.fromResource(name).getLines()

  def charsFromTextFile(name: String): Iterator[Char] =
    Source.fromResource(name).iter

  def intLinesFromTextFile(name: String): Iterator[Int] =
    linesFromTextFile(name).map(_.toInt)

}
