package aoc

import java.lang.Math.floorMod

object Day09 extends App {

  val marbles = 70918
  val elves = 464
  val scores = Array.fill(elves)(0)

  type Ring = (Vector[Int], Vector[Int])

  case class State(ring: Vector[Int], idx: Int) {
    def place(marble: Int) = marble match {
      case magic if magic % 23 == 0 =>
        val next = floorMod(idx - 7, ring.size)
        val (left, right) = ring splitAt next
        scores(magic % elves) += magic + ring(next)
        this.copy(ring = left ++ right.tail, idx = next)
      case muggle =>
        val next = floorMod(idx + 1, ring.size) + 1
        val (left, right) = ring splitAt next
        this.copy(ring = left ++ (muggle +: right), idx = next)
    }

    def reward(player: Int, inc: Int) =
      scores(player) += inc
  }

  val initial = State(Vector(0, 2, 1, 3), 3)

  val finalState = (4 to marbles + 1).foldLeft(initial)(_ place _)

  println(scores.max)

}