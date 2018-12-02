package aoc

import java.lang.Math.floorMod

object Day01 extends App {

  val data = DataSource.intLinesFromTextFile("day-1-input.txt").toStream
  lazy val sequence: Stream[Int] = data #::: sequence
  lazy val series = sequence.scan(0)(_ + _)
  lazy val cycle = series.take(T)

  val T = data.length
  val cycleSum = series(T)

  def partOne() = cycleSum

  def partTwoNaive() = StreamUtils.firstDuplicate(series).get

  def partTwoBetter() = StreamUtils.firstDuplicate(cycle) match {
    case Some(ans) => ans
    case None if cycleSum == 0 => 0
    case None => partTwoBetterContinuation()
  }

  def partTwoBetterContinuation() = {
    var (minIndex, minDelta, minFreq) = (Option.empty[Int], Option.empty[Int], Option.empty[Int])
    val groups = cycle.zipWithIndex.groupBy(x => floorMod(x._1, cycleSum)).values.filter(_.nonEmpty)
    for {group <- groups}
      group
        .sortBy(_._1)
        .reduce {
          (previous, current) =>
            val d = current._1 - previous._1
            val index = if (cycleSum > 0) previous._2 else current._2
            val f = if (cycleSum > 0) current._1 else previous._1
            if (minDelta.isEmpty || minDelta.exists(_ > d) || (minDelta.contains(d) && minIndex.exists(_ > index))) {
              minIndex = Some(index)
              minDelta = Some(d)
              minFreq = Some(f)
            }
            current
        }

    minFreq.get
  }

  println(partOne())
  private val naiveAnswer = partTwoNaive()
  private val fastAnswer = partTwoBetter()
  println(naiveAnswer)
  println(fastAnswer)
  assert(fastAnswer == naiveAnswer)
}

object StreamUtils {

  def firstDuplicate[T](xs: Stream[T]): Option[T] =
    xs.scanLeft(Set.empty[T])(_ + _)
      .zip(xs)
      .find { case (s, x) => s contains x }
      .map(_._2)

}
