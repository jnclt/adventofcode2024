#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/8

val lines = io.Source.fromFile("input.txt").getLines().toVector
val rowSpan = 0 until lines.size
val colSpan = 0 until lines.head.size

case class Position(x: Int, y: Int):
  def within: Boolean =
    rowSpan.contains(this.y) && colSpan.contains(this.x)
  def -(that: Position): Position =
    Position(this.x - that.x, this.y - that.y)
  def unary_- = Position(-this.x, -this.y)

case class Antenna(freq: Char, pos: Position)
val antennas = (for
  y <- rowSpan
  x <- colSpan
  if lines(y)(x) != '.'
yield Antenna(lines(y)(x), Position(x, y)))
  .groupMap(_.freq)(_.pos)
  .mapValues(_.toSet)

def antinodes1(left: Position, right: Position): Set[Position] =
  val diff = right - left
  Set(left - diff, right - diff).filter(_.within)

@scala.annotation.tailrec
def antinodesRec(
    pos: Position,
    diff: Position,
    acc: Set[Position]
): Set[Position] =
  val next = pos - diff
  if next.within then antinodesRec(next, diff, acc + next)
  else acc

def antinodes2(left: Position, right: Position): Set[Position] =
  val diff = right - left
  antinodesRec(left, diff, Set(left)) ++
    antinodesRec(right, -diff, Set(right))

def antinodesForFreq(antinodesFunc: (Position, Position) => Set[Position])(
    positions: Set[Position]
): Set[Position] =
  val pairs = (for
    p1 <- positions
    p2 <- positions
    if p1 != p2
  yield Set(p1, p2))
    .map(pair =>
      val left = pair.minBy(_.x)
      val right = pair.maxBy(_.x)
      (left, right)
    )

  pairs.map(antinodesFunc.tupled(_)).flatten

def allAntinodes(
    antinodesFunc: (Position, Position) => Set[Position]
): Set[Position] =
  antennas.mapValues(antinodesForFreq(antinodesFunc)).values.flatten.toSet

println(allAntinodes(antinodes1).size)
println(allAntinodes(antinodes2).size)
