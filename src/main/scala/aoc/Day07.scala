package aoc

import scala.annotation.tailrec

object Day07 extends App {

  val lines = DataSource.linesFromTextFile("day-7-input.txt").toList
  val tuples = lines.sorted.map(Steps.parse)
  val graph = tuples.groupBy(_._1).mapValues(_.map(_._2).toList).withDefaultValue(Nil)
  val reverse = tuples.groupBy(_._2).mapValues(_.map(_._1).toList).withDefaultValue(Nil)
  val sources = (graph.keySet diff reverse.keySet).toList.sorted
  val workers = 5
  def cost(task: Char): Int = task.toInt - 4

  @tailrec def kahn(finished: List[Char], tasks: List[Char]): List[Char] =
    tasks match {
      case Nil => finished
      case task :: tail if finished.contains(task) => kahn(finished, tail)
      case task :: tail if !reverse(task).forall(finished.contains) => kahn(finished, tail)
      case task :: tail => kahn(task :: finished, (graph(task) ++ tail).sorted)
    }

  def dependencies(graph: Map[Char, List[Char]], sourceTasks: List[Char]): List[Char] =
    kahn(Nil, sourceTasks).reverse

  @tailrec
  def schedule(atHand: List[Task], pending: List[Char], finished: List[Char], time: Int): Int =
    (atHand, pending) match {
      case (Nil, Nil) => time
      case (_, Nil) => time + atHand.map(_.cost).max
      case _ if atHand.size == workers =>
        atHand.sortBy(_.cost) match {
          case done :: doing =>
            val progress = done.cost
            schedule(doing.map(t => t.copy(cost = t.cost - progress)), pending, done.name +: finished, time + progress)
        }
      case _ =>
        pending.find(t => reverse(t).isEmpty || reverse(t).forall(finished.contains)) match {
          case Some(next) =>
            schedule(Task(next, time, cost(next)) +: atHand, pending.filter(_ != next), finished, time)
          case None =>
            atHand.sortBy(_.cost) match {
              case done :: doing =>
                val progress = done.cost
                schedule(doing.map(t => t.copy(cost = t.cost - progress)), pending, done.name +: finished, time + progress)
            }
        }
    }

  val order = dependencies(graph, sources)
  println(s"Part 1: ${order.mkString("")}")
  val totalTime = schedule(Nil, order, Nil, 0)
  print(s"Part 2: $totalTime")

}

case class Task(name: Char, start: Int, cost: Int)

object Steps {
  val Pattern = "^Step (.) must be finished before step (.) can begin.$".r

  def parse(s: String) = s match {
    case Pattern(from, to) => from.head -> to.head
  }
}
