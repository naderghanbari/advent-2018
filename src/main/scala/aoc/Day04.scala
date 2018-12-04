package aoc

object Day04 extends App {

  val lines = DataSource.linesFromTextFile("day-4-input.txt")
  val records = lines.toSeq.sorted.map(Record.parse)
  val space = 0 to 59

  val (table, _, _) = records.foldLeft((Map.empty[Int, List[Range]], Option.empty[Int], Option.empty[Int])) {
    case ((m, Some(id), Some(t1)), Begins(next, _)) =>
      (m.updated(id, (t1 until 60) +: m.getOrElse(id, Nil)), Some(next), None)
    case ((m, g, None), Falls(a)) =>
      (m, g, Some(a))
    case ((m, g@Some(id), Some(a)), Wakes(w)) =>
      (m + (id -> ((a until w) +: m.getOrElse(id, Nil))), g, None)
    case ((m, _, _), Begins(id, _)) =>
      (m, Some(id), None)
  }

  val (chosenOne, _) = table.mapValues(_.map(_.size).sum).maxBy(_._2)
  val hisWeakSpot = space.map(m => m -> table(chosenOne).count(_.contains(m))).maxBy(_._2)._1

  println(s"Part 1: ${chosenOne * hisWeakSpot}")

  val heatMap = table.flatMap { case (id, r) => space.map(m => (id, m, r.count(_.contains(m)))) }
  val (chosenTwo, hisHotSpot, _) = heatMap.maxBy(_._3)
  println(s"Part 2: ${chosenTwo * hisHotSpot}")

}

sealed trait Record
case class Begins(guardId: Int, minute: Int) extends Record
case class Falls(minute: Int) extends Record
case class Wakes(minute: Int) extends Record

object Record {
  val B = "^\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:(\\d\\d)\\]\\sGuard #(\\d+) begins shift$".r
  val W = "^\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:(\\d\\d)\\]\\swakes up$".r
  val F = "^\\[\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:(\\d\\d)\\]\\sfalls asleep$".r

  def parse(s: String): Record = s match {
    case B(min, id) => Begins(id.toInt, min.toInt)
    case W(min) => Wakes(min.toInt)
    case F(min) => Falls(min.toInt)
  }
}
