#!/usr/bin/env -S scala-cli shebang

// val size = 6
// val byteCount = 12
val size = 70
val byteCount = 1024
val span = 0 to size

case class Pos(x: Int, y: Int):
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)
  lazy val within: Boolean =
    span.contains(this.y) && span.contains(this.x)

val lines = io.Source.fromFile("input.txt").getLines
// val lines = io.Source.fromFile("sample.txt").getLines
val bytes = lines
  .take(byteCount)
  .map(l =>
    val s"$x,$y" = l: @unchecked
    Pos(x.toInt, y.toInt)
  )
  .toSet

val (start, end) = (Pos(0, 0), Pos(size, size))
val dirs = Set(Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))

import scala.collection.mutable.Map as mMap
import scala.collection.mutable.Set as mSet

val costs: mMap[Pos, Int] = mMap(start -> 0)
val visited = mSet.empty[Pos]

def neighbors(p: Pos): Set[Pos] =
  dirs
    .map(_ + p)
    .filter(n => n.within && !bytes(n) && !visited(n))

def visit(at: Pos): Unit =
  val newCost = costs(at) + 1
  neighbors(at).map(nbr =>
    costs.get(nbr) match
      case Some(c) if c < newCost => ()
      case _                      => costs(nbr) = newCost
  )
  visited.add(at)

def pprint(): Unit =
  for
    y <- span
    x <- span
  do
    val p = Pos(x, y)
    if bytes(p) then print("#")
    else if costs.contains(p) then print(costs(p) % 10)
    else print(".")
    if x == span.last then println()

// pprint()

while !visited(end) do
  val (next, _) = (costs.subtractAll(visited)).minBy((k, v) => v)
  visit(next)

println(costs(end))
