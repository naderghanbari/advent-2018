package aoc

object Day14 extends App {

  val input = DataSource.linesFromTextFile("day-14-input.txt").next.trim
  val (size, inputAsInt) = (input.length, input.toInt)

  def vectorize(xs: String) = xs.map(_.toString.toByte).toVector

  case class Gen(recipes: Vector[Byte], chefIndex: Int, sousIndex: Int)

  def nextGen(gen: Gen, genNr: Int): Gen = gen match {
    case Gen(rs, chef, sous) =>
      val (chefRecipe, sousRecipe) = (rs(chef), rs(sous))
      val newRecipes = rs ++ vectorize((chefRecipe + sousRecipe).toString)
      val newChefIndex = (chef + chefRecipe + 1) % newRecipes.size
      val newSousIndex = (sous + sousRecipe + 1) % newRecipes.size
      Gen(newRecipes, newChefIndex, newSousIndex)
  }

  val firstGen = Gen(recipes = Vector[Byte](3, 7), chefIndex = 0, sousIndex = 1)

  val partOne =
    Iterator
      .from(1)
      .scanLeft(firstGen)(nextGen)
      .dropWhile(_.recipes.size < inputAsInt + 10)
      .next
      .recipes
      .slice(inputAsInt, inputAsInt + 10)
      .mkString("")

  println("Part 1:")
  println(partOne)

  val Some(partTwo) =
    Iterator
      .from(1)
      .scanLeft(firstGen)(nextGen)
      .map { gen =>
        gen.recipes.size -> gen.recipes.takeRight(size + 1).mkString("").indexOf(input)
      }
      .collectFirst {
        case (total, idx) if idx >= 0 => total - size + idx - 1
      }

  println("Part 2:")
  println(partTwo)

}
