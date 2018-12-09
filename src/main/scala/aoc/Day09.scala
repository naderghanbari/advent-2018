package aoc

import java.util

import scala.collection.mutable

/**
  * First I used an immutable ring (consisting of two Vectors one flipped and one in
  * normal order, but then I realised an occasional reverse is needed so the amortized
  * complexity was not good (probably still sub-linear but definitely not constant).
  *
  * Then I tried ArrayDeque (from Scala 2.13.0-M4) and it worked but other solutions (day 3, 4, 8) broke
  * because of changes in Parallel collections).
  *
  * Final call: mutability to the rescue (Java LinkedList)
  * for now until I find a good immutable circular doubly-linked linked list.
  */
object Day09 extends App {

  val elves  = 464
  val scores = mutable.Map.empty[Int, Long].withDefaultValue(0L)

  val deck               = new util.LinkedList[Int]()
  def <<<                = deck offerLast deck.removeFirst()
  def >>>(whoCares: Int) = deck offerFirst deck.removeLast()

  def place(marble: Int) = if (marble % 23 == 0) placeMagic(marble) else placeMuggle(marble)

  def placeMagic(magic: Int) = {
    (1 to 7) foreach >>>
    scores(magic % elves) += magic + deck.removeLast()
    <<<
  }

  def placeMuggle(muggle: Int) = {
    <<<
    deck addLast muggle
  }

  ////////////////////////////////////////////////////

  val marbles = 70918
  deck push 0

  (1 to marbles).foreach(place)
  println(s"Part 1: ${scores.values.max}")

  (marbles + 1 to marbles * 100).foreach(place)
  println(s"Part 2: ${scores.values.max}")

}