#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/10

val topo = io.Source
  .fromFile("input.txt")
  .getLines()
  .map(_.toCharArray().map(_.asDigit).toVector)
  .toVector
val rowSpan = 0 until topo.size
val colSpan = 0 until topo.head.size

case class Pos(x: Int, y: Int):
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)

val dirs = Set(Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))

def within(p: Pos): Boolean =
  rowSpan.contains(p.y) && colSpan.contains(p.x)

def at(p: Pos): Int = topo(p.y)(p.x)

def neighbors(from: Pos): Set[Pos] =
  val nextVal = at(from) + 1
  dirs.map(from + _).filter(within).filter(at(_) == nextVal)

val trailheads = for
  y <- rowSpan
  x <- colSpan
  if topo(y)(x) == 0
yield Pos(x, y)

def trailends(from: Pos): Set[Pos] =
  if at(from) == 9 then Set(from)
  else neighbors(from).foldLeft(Set[Pos]())((acc, pos) => acc ++ trailends(pos))

val scores = trailheads.map(trailends).map(_.size)
println(scores.sum)

def trails(from: Pos): Int =
  if at(from) == 9 then 1
  else neighbors(from).foldLeft(0)((acc, pos) => acc + trails(pos))

val ratings = trailheads.map(trails)
println(ratings.sum)
