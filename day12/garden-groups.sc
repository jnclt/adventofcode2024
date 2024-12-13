#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/12

val plots =
  io.Source.fromFile("input.txt").getLines.map(_.toCharArray()).toArray
val rowSpan = 0 until plots.length
val colSpan = 0 until plots.head.length

case class Pos(r: Int, c: Int):
  def +(that: Pos) = Pos(this.r + that.r, this.c + that.c)

val dirs = Set(Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))

def within(p: Pos): Boolean =
  rowSpan.contains(p.r) && colSpan.contains(p.c)

def at(p: Pos): Char = plots(p.r)(p.c)

def neighbors(plot: Pos): Set[Pos] =
  val plant = at(plot)
  dirs.map(plot + _).filter(within).filter(at(_) == plant)

val nbrs = (for
  r <- rowSpan
  c <- colSpan
yield Pos(r, c))
  .map(p => (p -> neighbors(p)))
  .toMap

def visit(p: Pos, visited: Set[Pos]): Set[Pos] =
  (nbrs(p) -- visited).foldLeft(visited + p)((vs, n) => visit(n, vs))

def perimeter(region: Set[Pos]): Int =
  region.toSeq.map(4 - nbrs(_).size).sum

def split(perimeter: Set[Pos] => Int)(toVisit: Set[Pos], cost: Int): Int =
  if toVisit.isEmpty then cost
  else
    val visited = visit(toVisit.head, Set())
    val visitedCost = visited.size * perimeter(visited)
    split(perimeter)(toVisit -- visited, cost + visitedCost)

println(split(perimeter)(nbrs.keySet, 0))

def corners(region: Set[Pos])(p: Pos): Int =
  List(
    (Set(p + Pos(-1, 0), p + Pos(0, +1)) & region).isEmpty, // NE out
    (Set(p + Pos(+1, 0), p + Pos(0, +1)) & region).isEmpty, // SE out
    (Set(p + Pos(+1, 0), p + Pos(0, -1)) & region).isEmpty, // SW out
    (Set(p + Pos(-1, 0), p + Pos(0, -1)) & region).isEmpty, // NW out
    (Set(p + Pos(-1, 0), p + Pos(0, +1), p + Pos(-1, +1)) &~ region) == Set(
      p + Pos(-1, +1)
    ), // NE in
    (Set(p + Pos(+1, 0), p + Pos(0, +1), p + Pos(+1, +1)) &~ region) == Set(
      p + Pos(+1, +1)
    ), // SE in
    (Set(p + Pos(+1, 0), p + Pos(0, -1), p + Pos(+1, -1)) &~ region) == Set(
      p + Pos(+1, -1)
    ), // SW in
    (Set(p + Pos(-1, 0), p + Pos(0, -1), p + Pos(-1, -1)) &~ region) == Set(
      p + Pos(-1, -1)
    ) // NW in
  ).count(identity)

def perimeter2(region: Set[Pos]): Int =
  region.toList.map(corners(region.toSet)).sum

println(split(perimeter2)(nbrs.keySet, 0))
