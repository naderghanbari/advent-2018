package aoc

import scala.annotation.tailrec

/** Part 1: Immutable dynamic programming
  * Part 2: No backtrack is needed (due to the nature of the utility function by always expanding the
  * min node).
  */
object Day22 extends App {

  val Depth = 7305
  val Target = Region(13, 734)
  val searchZone = (0 to Target.x * 6) flatMap (x => 0 to Target.y * 6 map (x -> _)) map Region.tupled
  def levelOf(index: Int) = (index + Depth) % 20183

  val levels = (searchZone foldLeft Map.empty[Region, Int]) {
    case (map, Target) => map + (Target -> levelOf(0))
    case (map, r@Region(x, 0)) => map + (r -> levelOf(x * 16807))
    case (map, r@Region(0, y)) => map + (r -> levelOf(y * 48271))
    case (map, r@Region(x, y)) => map + (r -> levelOf(map(Region(x - 1, y)) * map(Region(x, y - 1))))
  }
  val typeOf = levels mapValues (_ % 3)

  case class Region(x: Int, y: Int) {
    def neighbors = Vector(copy(x = x - 1), copy(y = y - 1), copy(y = y + 1), copy(x = x + 1)).filter(levels.contains)
    def children(gear: Int) =
      neighbors
        .flatMap { neighbor =>
          (0 to 2)
            .filter(_ != typeOf(neighbor))
            .collect {
              case newGear if gear != typeOf(neighbor) && newGear != gear => (neighbor, newGear, 1 + 7)
              case newGear if gear != typeOf(neighbor) => (neighbor, newGear, 1)
            }
        }
  }

  val zone = 0 to Target.x flatMap (x => 0 to Target.y map (x -> _)) map Region.tupled
  println(s"Part 1: ${zone.map(typeOf).sum}")

  @tailrec def expand(frontier: Set[(Region, Int, Int)], shortest: (Region, Int) Map Int): Int = {
    val nodeToExpand = frontier minBy (_._3)
    val updatedFrontier = frontier - nodeToExpand
    val (current, gear, soFar) = nodeToExpand
    if ((current, gear) == (Target, 1)) return soFar
    if ((levels contains current) && shortest((current, gear)) >= soFar) {
      val (frontierUpdated, distUpdated) =
        current
          .children(gear)
          .collect {
            case it@(neighbor, neighborGear, cost)
              if !shortest.get((neighbor, neighborGear)).exists(_ <= soFar + cost) => it
          }
          .foldLeft((updatedFrontier, shortest)) {
            case ((uf, sd), (neighbor, neighborGear, cost)) =>
              uf + ((neighbor, neighborGear, soFar + cost)) -> (sd + ((neighbor, neighborGear) -> (soFar + cost)))
          }
      expand(frontierUpdated, distUpdated)
    }
    else
      expand(updatedFrontier, shortest)
  }

  val shortest = expand(Set((Region(0, 0), 1, 0)), Map((Region(0, 0), 1) -> 0))
  println(s"Part 2: $shortest")

}
