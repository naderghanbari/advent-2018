package aoc

object Day11 extends App {

  val SerialNumber = 1133

  val arr = Array.fill(300)(Array.fill(300)(0))
  for {
    x <- (1 to 300).par
    y <- 1 to 300
    l = level(x, y)
  } arr(x - 1)(y - 1) = l

  def squareIndices(size: Int) = (0 until size).flatMap(dx => (0 until size).map(dx -> _))

  def level(x: Int, y: Int) =
    (((x * x * y +
      x * 10 * y +
      x * SerialNumber +
      10 * x * y +
      100 * y +
      10 * SerialNumber) / 100) % 10) - 5

  def maxLevel(size: Int) = {
    val delta = squareIndices(size)
    val triplets = for {
      x <- (1 to (300 - size)).par
      y <- 1 to (300 - size)
      sum = delta.map { case (dx, dy) => arr(x + dx - 1)(y + dy - 1) }.sum
    } yield (x, y, sum)
    val maxTriplet = triplets.maxBy(_._3)
    (maxTriplet, size)
  }

  println("Part 1")
  println(maxLevel(3))

  val ((bestX, bestY, _), bestSize) = Iterator
    .from(1)
    .map(maxLevel)
    .takeWhile(_._1._3 >= 0)
    .take(299)
    .maxBy(_._1._3)

  println(s"Part 2")
  println(s"$bestX,$bestY,$bestSize")

}

