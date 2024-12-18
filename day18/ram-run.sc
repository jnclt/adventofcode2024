#!/usr/bin/env -S scala-cli shebang

val size = 70
val byteCount = 1024
val span = 0 to size

case class Pos(x: Int, y: Int):
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)
  lazy val within: Boolean =
    span.contains(this.y) && span.contains(this.x)

val lines = io.Source.fromFile("input.txt").getLines.toList
val allBytes = lines
  .map(l =>
    val s"$x,$y" = l: @unchecked
    Pos(x.toInt, y.toInt)
  )
val bytes = allBytes.take(byteCount).toSet

val (start, end) = (Pos(0, 0), Pos(size, size))
val dirs = Set(Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))

import scala.collection.mutable.Map as mMap
import scala.collection.mutable.Set as mSet

val costs: mMap[Pos, Int] = mMap(start -> 0)
val visited = mSet.empty[Pos]

def neighbors(p: Pos, bytes: Set[Pos], visited: mSet[Pos]): Set[Pos] =
  dirs
    .map(_ + p)
    .filter(n => n.within && !bytes(n) && !visited(n))

def visit(at: Pos): Unit =
  val newCost = costs(at) + 1
  neighbors(at, bytes, visited).map(nbr =>
    costs.get(nbr) match
      case Some(c) if c < newCost => ()
      case _                      => costs(nbr) = newCost
  )
  visited.add(at)

while !visited(end) do
  val (next, _) = (costs.subtractAll(visited)).minBy((k, v) => v)
  visit(next)

println(costs(end))

import scala.collection.mutable.Stack

def isReachable(length: Int): Boolean =
  val bytes = allBytes.take(length).toSet
  val visited = mSet.empty[Pos]
  val toVisit = Stack(start)
  while !visited(end) && !toVisit.isEmpty do
    val at = toVisit.pop()
    val nexts = neighbors(at, bytes, visited)
    visited.add(at)
    toVisit.pushAll(nexts)
  visited(end)

val len = (byteCount + 1 to allBytes.size).find(l => !isReachable(l))
println(allBytes(len.get - 1))
