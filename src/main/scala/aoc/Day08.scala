package aoc

/**
  * - Lists are persistent data structures so chopping them in the
  * recursion (or the foldLeft) makes sense (instead of using an array and
  * working with indices). No math needed with Lists to keep track of
  * start and end of each node: pure chop-chop shove-shove works!
  *
  * - `parse` returns the parsed tree and the residue of the provided tokens.
  */
object Day08 extends App {

  val input = DataSource.linesFromTextFile("day-8-input.txt").next()
  val numbers = input.split(" ").map(_.toInt).toList

  type Meta = List[Int]
  type Tokens = List[Int]
  type Children = List[Tree]

  case class Tree(node: Meta, children: Children)

  def folder(soFar: (Children, Tokens), x: Int) = parse(soFar._2) match {
    case (me, residue) => (me +: soFar._1) -> residue
  }

  def parse(tokens: Tokens): (Tree, Tokens) = {
    val (List(childNr, metaNr), body) = tokens splitAt 2
    val noChildrenYet = List.empty[Tree]
    val (kids, footer) = (1 to childNr).foldLeft(noChildrenYet -> body)(folder)
    val (meta, residue) = footer splitAt metaNr
    Tree(meta, kids.reverse) -> residue
  }

  def bfs(tree: Tree): Stream[Tree] = tree #:: tree.children.map(bfs).fold(Stream.Empty)(_ ++ _)

  val (tree, _) = parse(numbers)
  val metaEntriesCount = bfs(tree).map(_.node.sum).sum
  println(s"Part 1: $metaEntriesCount")
}
