package aoc

object Day22 extends App {

  val Depth = 7305
  val Target = Region(13, 734)

  case class Region(x: Int, y: Int)

  def levelOf(index: Int) = (index + Depth) % 20183
  def riskOf(level: Int) = level % 3

  val zone = 0 to Target.x flatMap (x => 0 to Target.y map (x -> _)) map Region.tupled

  val levels = (zone foldLeft Map.empty[Region, Int]) {
    case (map, Target) => map + (Target -> levelOf(0))
    case (map, r@Region(x, 0)) => map + (r -> levelOf(x * 16807))
    case (map, r@Region(0, y)) => map + (r -> levelOf(y * 48271))
    case (map, r@Region(x, y)) => map + (r -> levelOf(map(Region(x - 1, y)) * map(Region(x, y - 1))))
  }

  println(s"Part 1: ${levels.values.map(riskOf).sum}")
}
