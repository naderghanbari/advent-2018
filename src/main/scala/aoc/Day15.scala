package aoc

import scala.Function.tupled
import scala.collection.{immutable, mutable}

object Day15 extends App {

  val terrain = DataSource.linesFromTextFile("day-15-input.txt").map(_.toArray).toArray

  val (height, width) = (terrain.length, terrain.head.length)
  val (ver, hor) = ((0 until height).view, (0 until width).view)
  def isWall(l: Location) = resident(l) == '#'
  def squares = ver.flatMap(y => hor.map(Location(y, _)))

  def field = squares.filterNot(isWall).toVector
  val warriors = mutable.Map.empty[Location, Warrior]
  val graph = (field, field.map(adjacent)).zipped.toMap

  field
    .collect {
      case l if hasElf(l) => warriors + (l -> Elf())
      case l if hasGoblin(l) => warriors + (l -> Goblin())
    }

  def round() = {
    val candidates = warriors.keySet.toVector.sorted
    def inRange(current: Location): immutable.Seq[(Location, Warrior)] = {
      warriors
        .collect {
          case (l, enemy) if graph(current).contains(l) && warriors(current).isFoe(enemy) => (l, enemy)
        }
        .toList
        .sortBy(it => (it._1, it._2.HP))
    }

    def attack(l: Location) = {
      val hurt = warriors(l).hurt
      if (hurt.isDead) warriors -= l else warriors.update(l, hurt)
    }

    case class Traversal(visited: Seq[Location], level: Map[Location, Int], parent: Map[Location, Option[Location]])

    def shortestPathToEnemy(l: Location) = {

      def iter(current: Location, visited: Set[Location], path: List[Location]) = {
        val neighbors = graph(current)
        neighbors.find(l => warriors(current).isFoe(warriors(l)))
      }
    }

    candidates.foreach { current =>
      inRange(current) match {
        case (weakest, _) :: _ => attack(weakest)
        case Nil =>
      }
    }
  }

  def resident(l: Location) = terrain(l.y)(l.x)
  def isEmpty(l: Location) = resident(l) == '.'
  def hasElf(l: Location) = resident(l) == 'E'
  def hasGoblin(l: Location) = resident(l) == 'G'
  def adjacent(l: Location) = l.neighbors.filterNot(isWall)

}

sealed trait Warrior {
  val AP = 3
  val HP: Int
  def isFoe(that: Warrior) = (this, that) match {
    case (_: Goblin, _: Elf) | (_: Elf, _: Goblin) => true
    case _ => false
  }
  def hurt: Warrior = this match {
    case Goblin(hp) => Goblin(hp - AP)
    case Elf(hp) => Elf(hp - AP)
  }
  def isDead: Boolean = HP < 0
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

