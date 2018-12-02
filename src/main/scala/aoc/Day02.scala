package aoc

/** Compare the two by eye please ;)
  *
  * This is dumb but fast (still a naive algorithm, searching for a better one
  * see the future commits :D we are all time travelers by now, eh ?
  *
  * The combination of `.view` and `collectFirst` makes it possible to return the result as soon as it's found.
  */
object Day02 extends App {

  val data = DataSource.linesFromTextFile("day-2-input.txt").toSeq

  val counts = data.par.map(_.groupBy(identity).values.map(_.length).toSet)
  val product = counts.count(_.contains(2)) * counts.count(_.contains(3))
  println(product)

  def matching: Seq[String] PartialFunction (String, String) = {
    case Seq(xs, ys) if xs.zip(ys).count { case (x, y) => x != y } == 1 => (xs, ys)
  }

  val Some((first, second)) = data.sorted.view.sliding(2).collectFirst(matching)
  println(first)
  println(second)

}
