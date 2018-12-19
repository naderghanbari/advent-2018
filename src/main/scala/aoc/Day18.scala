package aoc

import aoc.DataSource.linesFromTextFile

object Day18 extends App {

  type State = Acre Map Char

  val input = linesFromTextFile("day-18-input.txt").map(_.toVector.zipWithIndex).toVector.zipWithIndex
  val terrain = input.flatMap { case (rows, y) => rows.map { case (content, x) => Acre(y, x) -> content } }.toMap
  val initial = terrain
  val around = (-1 to 1).flatMap(dy => (-1 to 1).map(dy -> _)).toSet - (0 -> 0)

  case class Acre(y: Int, x: Int) {
    lazy val neighbors = around.toVector map { case (dy, dx) => Acre(y + dy, x + dx) } filter terrain.contains
    def adj(state: State) = neighbors.map(state).groupBy(identity).mapValues(_.size).withDefaultValue(0)
  }

  def change(state: State)(acre: Acre, contents: Char) = contents match {
    case '.' if acre.adj(state)('|') >= 3 => '|'
    case '.' => '.'
    case '|' if acre.adj(state)('#') >= 3 => '#'
    case '|' => '|'
    case '#' if acre.adj(state)('#') >= 1 && acre.adj(state)('|') >= 1 => '#'
    case '#' => '.'
  }

  def tick(state: State): State = state.par.map { case (acre, contents) => acre -> change(state)(acre, contents) }.seq

  def valueMap(state: State) = state.values.groupBy(identity).mapValues(_.size).withDefaultValue(0)
  def valueOf(state: State) = valueMap(state)('|') * valueMap(state)('#')

  val states = Stream.from(1).scanLeft(initial)((now, _) => tick(now))
  println("Part 1: " + valueOf(states(10)))

  val Some((again, second)) = firstDuplicateWithIndex(states)

  // tiny bit of rework, could have been done in `firstDuplicateWithIndex`, but everything is memoized so it's fast
  val Some(first) = states.zipWithIndex.collectFirst { case (orig, fst) if orig == again => fst }

  val rewind = first + (1000000000 - first) % (second - first)

  println("Part 2: " + valueOf(states(rewind)))

  def firstDuplicateWithIndex[T](xs: Stream[T]): Option[(T, Int)] =
    xs.scanLeft(Set.empty[T])(_ + _).zip(xs.zipWithIndex).collectFirst { case (s, (x, i)) if s contains x => x -> i }

}
