package aoc

object Day20 extends App {

  type Room = (Int, Int)
  val Dir = Map('N' -> (0, -1), 'E' -> (1, 0), 'S' -> (0, 1), 'W' -> (-1, 0))
  def nextDoor(dir: Char)(room: Room) = (room, Dir(dir)) match { case ((x, y), (dx, dy)) => x + dx -> (y + dy) }

  def folder(state: State, c: Char) = c match {
    case '(' => state copy (stack = state.room :: state.stack)
    case '|' => state copy (room = state.stack.head)
    case ')' => state copy (room = state.stack.head, stack = state.stack.tail)
    case dir =>
      val (neighbor, forward) = (nextDoor(dir)(state.room), state.map(state.room) + 1)
      val diversion = state.map(neighbor)
      val updatedEntry = neighbor -> (if (diversion > 0) forward min diversion else forward)
      state copy (room = neighbor, map = state.map + updatedEntry)
  }

  val input = DataSource.linesFromTextFile("day-20-input.txt").next.trim.toStream.drop(1).dropRight(1)
  val initial = State((0, 0), List.empty[Room], Map.empty[Room, Int].withDefaultValue(0))

  case class State(room: Room, stack: List[Room], map: Room Map Int)
  val result = (input foldLeft initial) { (p ,char) => folder(p, char) }


  println("Part 1: " + result.map.values.max)

}
