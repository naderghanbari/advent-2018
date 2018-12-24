package aoc

object Day23 extends App {

  val bots = DataSource.linesFromTextFile("day-23-input.txt").map(Bot.parse).toVector
  val strongest = bots maxBy (_.r)
  val inRange = bots count strongest.sees
  println(s"Part 1: $inRange")

}

case class Bot(x: Int, y: Int, z: Int, r: Int) {
  def distance(that: Bot) = List(x - that.x, y - that.y, z - that.z).map(math.abs).sum
  def sees(that: Bot) = distance(that) <= r
}

object Bot {
  val Pattern = "^pos=<(-?\\d+),(-?\\d+),(-?\\d+)>,\\s+r=(\\d+)$".r
  val parse: String => Bot = { case Pattern(x, y, z, r) => Bot(x.toInt, y.toInt, z.toInt, r.toInt) }
}
