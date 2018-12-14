package aoc


object Day13 extends App {

  val land = DataSource.linesFromTextFile("day-13-input.txt").map(_.toArray).toArray

  val initialCarts = land.zipWithIndex.flatMap { case (line, y) => line.zipWithIndex
    .collect {
      case ('>', x) => Cart(y, x, '>', 0)
      case ('<', x) => Cart(y, x, '<', 0)
      case ('^', x) => Cart(y, x, '^', 0)
      case ('v', x) => Cart(y, x, 'v', 0)
    }
  }

  initialCarts.collect {
    case Cart(y, x, '>' | '<', _) => land(y)(x) = '-'
    case Cart(y, x, '^' | 'v', _) => land(y)(x) = '|'
  }

  def nextGen(carts: Set[Cart]): (Set[Cart], Set[Collision]) =
    carts.toVector.sorted
      .foldLeft((Set.empty[Cart], Set.empty[Collision])) {
        case ((buff, collisions), current) =>
          if (collisions.contains(Collision(current.y, current.x))) {
            println("ALREADY DEAD")
            (buff.filterNot(_.coordinates == current.coordinates), collisions + Collision(current.y, current.x))
          }
          else {
            val moving = move(current)
            buff.find(moving.collides) match {
              case Some(kaput) =>
                (buff.filterNot(_.coordinates == kaput.coordinates), collisions + Collision(moving.y, moving.x))
              case None => (buff + moving, collisions)
            }
          }
      }

  def move(current: Cart) = (current, land(current.y)(current.x)) match {
    case (Cart(_, x, '>', _), '-') => current.copy(x = x + 1)
    case (Cart(_, x, '>', 1), '+') => current.copy(x = x + 1, turned = 2)
    case (Cart(y, _, '>', _), '/') => current.copy(y = y - 1, dir = '^')
    case (Cart(y, _, '>', 0), '+') => current.copy(y = y - 1, dir = '^', turned = 1)
    case (Cart(y, _, '>', _), '\\') => current.copy(y = y + 1, dir = 'v')
    case (Cart(y, _, '>', 2), '+') => current.copy(y = y + 1, dir = 'v', turned = 0)

    case (Cart(_, x, '<', _), '-') => current.copy(x = x - 1)
    case (Cart(_, x, '<', 1), '+') => current.copy(x = x - 1, turned = 2)
    case (Cart(y, _, '<', _), '/') => current.copy(y = y + 1, dir = 'v')
    case (Cart(y, _, '<', 0), '+') => current.copy(y = y + 1, dir = 'v', turned = 1)
    case (Cart(y, _, '<', _), '\\') => current.copy(y = y - 1, dir = '^')
    case (Cart(y, _, '<', 2), '+') => current.copy(y = y - 1, dir = '^', turned = 0)

    case (Cart(y, _, '^', _), '|') => current.copy(y = y - 1)
    case (Cart(y, _, '^', 1), '+') => current.copy(y = y - 1, turned = 2)
    case (Cart(_, x, '^', _), '\\') => current.copy(x = x - 1, dir = '<')
    case (Cart(_, x, '^', 0), '+') => current.copy(x = x - 1, dir = '<', turned = 1)
    case (Cart(_, x, '^', _), '/') => current.copy(x = x + 1, dir = '>')
    case (Cart(_, x, '^', 2), '+') => current.copy(x = x + 1, dir = '>', turned = 0)

    case (Cart(y, _, 'v', _), '|') => current.copy(y = y + 1)
    case (Cart(y, _, 'v', 1), '+') => current.copy(y = y + 1, turned = 2)
    case (Cart(_, x, 'v', _), '\\') => current.copy(x = x + 1, dir = '>')
    case (Cart(_, x, 'v', 0), '+') => current.copy(x = x + 1, dir = '>', turned = 1)
    case (Cart(_, x, 'v', _), '/') => current.copy(x = x - 1, dir = '<')
    case (Cart(_, x, 'v', 2), '+') => current.copy(x = x - 1, dir = '<', turned = 0)
  }

  private val beforeFirstTick = initialCarts.sorted.toVector

  val Some(collision) = Iterator
    .continually(1)
    .scanLeft((beforeFirstTick.toSet, Set.empty[Collision])) { case ((gen, _), _) => nextGen(gen) }
    .find(_._2.nonEmpty)
    .flatMap(_._2.headOption)

  println("Part 1:")
  println(collision)

  val result = Iterator
    .continually(1)
    .scanLeft((beforeFirstTick.toSet, Set.empty[Collision])) {
      case ((gen, collided), _) =>
        nextGen(gen)
    }
    .dropWhile(_._1.size > 1)
    .next

  println(result)
}

case class Collision(y: Int, x: Int)

case class Cart(y: Int, x: Int, dir: Char, turned: Int) {
  val coordinates = (x, y)
  def collides(that: Cart) = coordinates == that.coordinates
}

object Cart {
  implicit val topLeftOrdering: Ordering[Cart] = Ordering.by(c => (c.y, c.x))
}
