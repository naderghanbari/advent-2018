package aoc

import aoc.ClayVein._

import scala.collection.mutable

/**
  * No way around the branching factor (StackOverFlow) here, one could move the concern
  * to heap of course (by using a Trampoline e.g.) but one would run out of memory eventually
  * when an exponential explosion happens. Quantum computing is on the horizon and one could almost
  * hope that it'll take a log of everything (time or space complexity) :D
  *
  * On a more serious note, I couldn't make this work with immutable data structures
  * in a reasonable amount of time.
  */
object Day17 extends App {

  import Dir._

  val input = DataSource.linesFromTextFile("day-17-input.txt").toStream
  val (rows, cols) = (input.collect(asRow).sortBy(_.y), input.collect(asCol).sortBy(_.x))
  val minY = rows.map(_.y).min min cols.map(_.top).min
  val maxY = rows.map(_.y).max max cols.map(_.bottom).max
  val inRange = (p: Place) => p.y >= minY && p.y <= maxY

  val rowPlaces = rows.flatMap(row => (row.left to row.right).map(x => Place(row.y, x)))
  val colPlaces = cols.flatMap(col => (col.top to col.bottom).map(y => Place(y, col.x)))
  val isClay = (rowPlaces ++ colPlaces).toSet

  val atRest = mutable.Set.empty[Place]
  val wet = mutable.Set.empty[Place]

  def unfold(current: Place, dir: Dir): Boolean = {
    wet += current
    val below = current neighbor Down
    var (left, right) = (current neighbor Left, current neighbor Right)

    if (!isClay(below)) {
      if (!wet(below) && (below.y >= 1) && (below.y <= maxY))
        unfold(below, Down)
      if (!atRest(below))
        return false
    }

    val noRoomLeftInLeft = isClay(left) || !wet(left) && unfold(left, Left)
    val noRoomLeftInRight = isClay(right) || !wet(right) && unfold(right, Right)
    if ((dir == Down) && noRoomLeftInLeft && noRoomLeftInRight) {
      atRest += current
      while (wet(left)) {
        atRest += left
        left = left neighbor Left
      }
      while (wet(right)) {
        atRest += right
        right = right neighbor Right
      }
    }

    dir match {
      case Left => noRoomLeftInLeft || isClay(left)
      case Right => noRoomLeftInRight || isClay(right)
      case _ => false
    }
  }

  val spring = Place(y = 0, x = 500)
  unfold(spring, Down)

  val partOne = wet.union(atRest).count(inRange)
  println("Part 1: " + partOne)

  val partTwo = atRest.count(inRange)
  println("Part 2: " + partTwo)

}

sealed trait ClayVein
object ClayVein {
  val RowPattern = "^y=(\\d+), x=(\\d+)\\.\\.(\\d+)$".r
  val asRow: String PartialFunction Row = {
    case RowPattern(y, left, right) => Row(y.toInt, left.toInt, right.toInt)
  }

  val ColPattern = "^x=(\\d+), y=(\\d+)\\.\\.(\\d+)$".r
  val asCol: String PartialFunction Col = {
    case ColPattern(x, top, bottom) => Col(x.toInt, top.toInt, bottom.toInt)
  }
}

case class Row(y: Int, left: Int, right: Int) extends ClayVein
case class Col(x: Int, top: Int, bottom: Int) extends ClayVein

object Dir extends Enumeration {
  type Dir = Value
  val Up, Down, Left, Right = Value
}

case class Place(y: Int, x: Int) {
  def neighbor(dir: Dir.Dir) = dir match {
    case Dir.Up => Place(y - 1, x)
    case Dir.Down => Place(y + 1, x)
    case Dir.Left => Place(y, x - 1)
    case Dir.Right => Place(y, x + 1)
  }
}