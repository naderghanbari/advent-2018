package aoc

import java.lang.Math.floorMod

object Day01Faster extends App {

  val data = DataSource.intLinesFromTextFile("day-1-input.txt").toStream

  lazy val sequence: Stream[Int] = data #::: sequence
  lazy val series = sequence.scan(0)(_ + _)
  lazy val cycle = series.take(T)
  val T = data.length
  val cycleSum = series(T)

  val fastAnswer = StreamUtils.firstDuplicate(cycle) match {
    case Some(ans) => ans
    case None if cycleSum == 0 => 0
    case None => groupSearch()
  }

  def groupSearch() = {
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

  println("Part 2: " + fastAnswer)

}
