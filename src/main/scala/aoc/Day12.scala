package aoc

/** This thing should stabilize in terms of size but then probably a cyclic group
  * begins.
  *
  * As the size of the stream of ints is unbounded, just a heuristic is used here.
  * I stop when 15 diffs are found to be the same. The second derivative (discrete difference)
  * will stabilize, otherwise we wouldn't be given this problem ;) If first derivative
  * didn't work I would have given the second derivative a try. It's enough to show
  * an nth derivative stabilizes after a while, and the rest is just a higher order series.
  *
  * As for each generation it's enough to add 4 at each end and prune false values at the end. 5 is not
  * needed because 5 false consecutive values never grow to true, otherwise the dead army would arise from ashes and
  * they will propagate till minus and plus infinity!!!
  */
object Day12 extends App {

  val (plantsLine :: _, patternLines) = DataSource.linesFromTextFile("day-12-input.txt").toList.splitAt(2)

  type Generation = Vector[(Boolean, Int)]
  type Pats = Seq[(Vector[Boolean], Boolean)]

  val allPats = patternLines.map { l => l.take(5).toVector.map(parse) -> parse(l.last) }
  val usefulPats = allPats.filter(_._2).map(_._1)

  val firstGen = plantsLine.drop(15).toVector.map(parse).zipWithIndex

  def parse(char: Char): Boolean = char match {
    case '#' => true
    case '.' => false
  }

  def nextGen(generation: Generation, ignore: Int): Generation = {
    val (firstIndex, lastIndex) = (generation.head._2, generation.last._2)
    val prefix = (1 to 4).map(firstIndex - _).map(false -> _).toVector.reverse
    val suffix = (1 to 4).map(lastIndex + _).map(false -> _).toVector
    val prev = prefix ++ generation ++ suffix
    val next = prev.sliding(5).map(it => usefulPats.contains(it.map(_._1)) -> (it.head._2 + 2)).toVector
    val pruned = next.dropWhile(!_._1).reverse.dropWhile(!_._1).reverse
    pruned
  }

  def value(generation: Generation) = generation.withFilter(_._1).map(_._2).sum

  val partOneResult =
    Iterator
      .from(1)
      .scanLeft(firstGen)(nextGen)
      .map(value)
      .drop(20)
      .next

  println("Part 1:")
  println(partOneResult)

  val Some(((sumSoFar, repeat), cycleBeginOneBased)) =
    Iterator
      .from(1)
      .scanLeft(firstGen)(nextGen)
      .map(value)
      .sliding(2)
      .map { case Seq(prev, curr) => (curr, curr - prev) }
      .zipWithIndex
      .sliding(20)
      .collectFirst { case seq if seq.map(_._1._2).distinct.size == 1 => seq.head }

  val cycleIndex = cycleBeginOneBased - 1
  private val repCount = 50 * 1000000000L - cycleIndex

  println("Part 2:")
  println(sumSoFar + repeat * repCount)

}
