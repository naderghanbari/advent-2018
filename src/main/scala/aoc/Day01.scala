package aoc

object Day01 extends App {

  val data = DataSource.intLinesFromTextFile("day-1-input.txt").toStream

  lazy val sequence: Stream[Int] = data #::: sequence
  lazy val series = sequence.scan(0)(_ + _)

  println("Part 1: " + data.sum)
  println("Part 2: " + StreamUtils.firstDuplicate(series).get)

}

object StreamUtils {

  def firstDuplicate[T](xs: Stream[T]): Option[T] =
    xs.scanLeft(Set.empty[T])(_ + _).zip(xs).find { case (s, x) => s contains x }.map(_._2)

}
