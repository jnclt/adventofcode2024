#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/16

// val grid = io.Source.fromFile("sample.txt").getLines.toList
// val grid = io.Source.fromFile("sample2.txt").getLines.toList
val grid = io.Source.fromFile("input.txt").getLines.toList

val rowSpan = 0 until grid.size
val colSpan = 0 until grid.head.size

case class Pos(x: Int, y: Int):
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)
  def *(f: Int) = Pos(this.x * f, this.y * f)
  lazy val within: Boolean =
    rowSpan.contains(this.y) && colSpan.contains(this.x)

case class Step(from: Pos, move: Pos):
  lazy val to = this.from + this.move

val walls = (for
  y <- rowSpan
  x <- colSpan
  if grid(y)(x) == '#'
yield Pos(x, y)).toSet

val sy = grid.indexWhere(_.find(_ == 'S').isDefined)
val sx = grid(sy).indexWhere(_ == 'S')
val start = Pos(sx, sy)

val ey = grid.indexWhere(_.find(_ == 'E').isDefined)
val ex = grid(ey).indexWhere(_ == 'E')
val end = Pos(ex, ey)

val dirs = Set(Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))

def neighbors(s: Step): Set[Step] =
  (dirs - s.move * -1)
    .map(Step(s.to, _))
    .filter(_.to.within)
    .filter(n => !walls(n.to))

import scala.collection.mutable.Map as mMap
import scala.collection.mutable.Stack

val costs = mMap(start -> 0)
val toTraverse = Stack(Step(start + Pos(-1, 0), Pos(1, 0)))

def traverseIter(at: Step): Set[Step] =
  neighbors(at).filter(nbr =>
    val newCost =
      costs(at.to) + 1 + (if at.move == nbr.move then 0 else 1000)
    if costs.get(nbr.to).map(_ <= newCost).getOrElse(false) then false
    else
      costs(nbr.to) = newCost
      true
  )

while !toTraverse.isEmpty do
  val at = toTraverse.pop()
  val nexts = traverseIter(at)
  toTraverse.pushAll(nexts)

val endCost = costs(end)
println(endCost)
