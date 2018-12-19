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
    case '.' if acre.adj(state)('|') >= 3                              => '|'
    case '.'                                                           => '.'
    case '|' if acre.adj(state)('#') >= 3                              => '#'
    case '|'                                                           => '|'
    case '#' if acre.adj(state)('#') >= 1 && acre.adj(state)('|') >= 1 => '#'
    case '#'                                                           => '.'
  }

  def tick(state: State): State = state.par.map { case (acre, contents) => acre -> change(state)(acre, contents) }.seq

  def valueMap(state: State) = state.values.groupBy(identity).mapValues(_.size).withDefaultValue(0)

  val finalState =
    Iterator
      .from(1)
      .take(10)
      .foldLeft(initial)((now, _) => tick(now))

  val partOne = valueMap(finalState)('|') * valueMap(finalState)('#')
  println("Part 1: " + partOne)

}
