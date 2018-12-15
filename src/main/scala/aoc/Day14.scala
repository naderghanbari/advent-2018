package aoc

object Day14 extends App {

  val input = DataSource.linesFromTextFile("day-14-input.txt").next.trim.toInt

  def combine(r1: Byte, r2: Byte): Vector[Byte] = (r1 + r2).toString.map(_.toString.toByte).toVector

  case class Gen(recipes: Vector[Byte], chefIndex: Int, sousIndex: Int)

  val firstGen = Gen(Vector[Byte](3, 7), chefIndex = 0, sousIndex = 1)

  def nextGen(gen: Gen, genNr: Int): Gen = gen match {
    case Gen(rs, chef, sous) =>
      val (chefRecipe, sousRecipe) = (rs(chef), rs(sous))
      val newRecipes = rs ++ combine(chefRecipe, sousRecipe)
      val newChefIndex = (chef + chefRecipe + 1) % newRecipes.size
      val newSousIndex = (sous + sousRecipe + 1) % newRecipes.size
      Gen(newRecipes, newChefIndex, newSousIndex)
  }

  val result =
    Iterator
      .from(1)
      .scanLeft(firstGen)(nextGen)
      .dropWhile(_.recipes.size < input + 10)
      .next
      .recipes
      .slice(input, input + 10)
      .mkString("")

  println("Part 1:")
  println(result)

}