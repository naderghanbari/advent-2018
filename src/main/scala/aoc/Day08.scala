package aoc

/**
  * - Lists are persistent data structures so chopping them in the
  * recursion (or the foldLeft) makes sense (instead of using an array and
  * working with indices). No math needed with Lists to keep track of
  * start and end of each node: pure chop-chop shove-shove works!
  *
  * - `parse` returns the parsed tree and the residue of the provided tokens.
  * - Vector provides eC (effectively constant time) for indexed access and append, so
  * it's a good choice for children (and it's immutable!).
  */
object Day08 extends App {

  val input = DataSource.linesFromTextFile("day-8-input.txt").next()
  val numbers = input.split(" ").map(_.toInt).toList
  type Meta = List[Int]
  type Tokens = List[Int]
  type Children = Vector[Tree]
  case class Tree(node: Meta, children: Children, value: Int)

  def folder(soFar: (Children, Tokens), x: Int) = parse(soFar._2) match {
    case (me, residue) => (soFar._1 :+ me) -> residue
  }

  def parse(tokens: Tokens): (Tree, Tokens) = {
    val (List(childNr, metaNr), body) = tokens splitAt 2
    val noChildrenYet = Vector.empty[Tree]
    val (children, footer) = (1 to childNr).foldLeft(noChildrenYet -> body)(folder)
    val (meta, residue) = footer splitAt metaNr
    val childMetaValue = children.map(_.value).applyOrElse(_: Int, (_: Int) => 0)
    val valueComponents = if (childNr == 0) meta else meta.map(_ - 1).map(childMetaValue)
    Tree(meta, children, valueComponents.sum) -> residue
  }

  def bfs(tree: Tree): Stream[Tree] = tree #:: tree.children.map(bfs).fold(Stream.Empty)(_ ++ _)

  val (tree, _) = parse(numbers)
  val metaEntriesCount = bfs(tree).map(_.node.sum).sum
  println(s"Part 1: $metaEntriesCount")
  println(s"Part 2: ${tree.value}")
}
