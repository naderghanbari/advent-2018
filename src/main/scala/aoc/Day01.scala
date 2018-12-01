package aoc

object Day01 extends App {

  val data = DataSource.intLinesFromTextFile("day-1-input.txt").toStream
  lazy val sequence: Stream[Int] = data #::: sequence

  def partOne() = data.sum

  def partTwoNaive() = firstDuplicate(sequence.scan(0)(_ + _)).get

  def firstDuplicate[T](xs: Stream[T]): Option[T] =
    xs.scanLeft(Set.empty[T])(_ + _)
      .zip(xs)
      .find { case (s, x) => s contains x }
      .map(_._2)

  println(partOne())
  println(partTwoNaive())

}
