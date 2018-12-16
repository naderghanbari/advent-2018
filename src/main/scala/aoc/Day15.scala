package aoc

import scala.Function.tupled

object Day15 extends App {

  type Square = (Char, Option[Warrior])

  val terrain: Array[Array[Square]] =
    DataSource.linesFromTextFile("day-15-input.txt")
      .map(
        _.toArray.map {
          case w@'#' => w -> Option.empty[Warrior]
          case n@'.' => n -> Option.empty[Warrior]
          case g@'G' => g -> Some(Goblin())
          case e@'E' => e -> Some(Elf())
        }
      )
      .toArray

  val (height, width) = (terrain.length, terrain.head.length)
  val (ver, hor) = ((0 until height).view, (0 until width).view)
  def squares = ver.flatMap(y => hor.map(Location(y, _)))
  def field = squares.filterNot(isWall).toVector
  val graph = (field, field.map(adjacent)).zipped.toMap
  def warriorLocations = field.filter(hasWarrior)

  def resident(l: Location) = terrain(l.y)(l.x)
  def isWall(l: Location) = resident(l)._1 == '#'
  def isEmpty(l: Location) = resident(l)._1 == '.'
  def hasElf(l: Location) = resident(l)._1 == 'E'
  def hasGoblin(l: Location) = resident(l)._1 == 'G'
  def hasWarrior(l: Location) = resident(l)._2.isDefined
  def adjacent(l: Location) = l.neighbors.filterNot(isWall)

  def inRange(square: Square) =
    warriorLocations
      .map(resident)
      .filter(it => it._2.exists(square._2.get.isFoe))
      .collect { case enemy if adjacent(square.l).contains(enemy.l) => enemy }
      .sorted[Warrior]
      .toList

  def move(w: Warrior): Unit = inRange(w) match {
    case weakest =>
    case
  }

  println(warriors.head)
  println(inRange(Goblin(Location(15, 29))))
  println(graph(Location(20, 24)))

}

sealed trait Warrior {
  val AP = 3
  val HP: Int
  def isFoe(that: Warrior) = (this, that) match {
    case (_: Goblin, _: Elf) | (_: Elf, _: Goblin) => true
    case _ => false
  }
}

case class Goblin(HP: Int = 300) extends Warrior
case class Elf(HP: Int = 300) extends Warrior

case class Location(y: Int, x: Int) {
  def around = Vector((-1, 0), (0, -1), (0, 1), (1, 0))
  def neighbors = around.map { case (dy, dx) => (y + dy, x + dx) }.map(tupled(Location.apply))
}

object Location {
  implicit val readingOrder: Ordering[Location] = Ordering.by(l => (l.y, l.x))
}

case class Field(adj: Location => Seq[Location])
