#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/8

val lines = io.Source.fromFile("input.txt").getLines().toVector
val rowSpan = 0 until lines.size
val colSpan = 0 until lines.head.size

case class Position(x: Int, y: Int):
  def within: Boolean =
    rowSpan.contains(this.y) && colSpan.contains(this.x)

case class Antenna(freq: Char, pos: Position)
val antennas = (for
  y <- rowSpan
  x <- colSpan
  if lines(y)(x) != '.'
yield Antenna(lines(y)(x), Position(x, y)))
  .groupMap(_.freq)(_.pos)
  .mapValues(_.toSet)

def antinodes1(left: Position, right: Position): Set[Position] =
  val dx = right.x - left.x
  val dy = right.y - left.y
  Set(
    Position(left.x - dx, left.y - dy),
    Position(right.x + dx, right.y + dy)
  ).filter(_.within)

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

val allAntinodes1 =
  antennas.mapValues(antinodesForFreq(antinodes1)).values.flatten.toSet
println(allAntinodes1.size)
