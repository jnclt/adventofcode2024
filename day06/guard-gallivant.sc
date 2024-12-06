#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/6

val lines = io.Source.fromFile("input.txt").getLines.toVector
// val lines = io.Source.fromFile("sample.txt").getLines.toVector
val rowRange = Range(0, lines.size)
val colRange = Range(0, lines.head.size)
val obstacles = for
  row <- rowRange
  col <- colRange
  if lines(row)(col) == '#'
yield (row, col)
val startRow = lines.indexWhere(_.contains('^'))
val startCol = lines(startRow).indexOf('^')

type Position = (Int, Int)
type Direction = (Int, Int)

def turnRight(dir: Direction): Direction = dir match
  case (0, 1)  => (1, 0) // > -> v
  case (1, 0)  => (0, -1) // v -> <
  case (0, -1) => (-1, 0) // < -> ^
  case (-1, 0) => (0, 1) // ^ -> >

def step(pos: Position, dir: Direction): (Position, Direction) =
  val ahead = (pos._1 + dir._1, pos._2 + dir._2)
  if obstacles.contains(ahead) then (pos, turnRight(dir))
  else (ahead, dir)

def walk(
    pos: Position,
    dir: Direction,
    walked: List[Position]
): List[Position] =
  if !(rowRange.contains(pos._1) && colRange.contains(pos._2)) then walked
  else
    val (nextPos, nextDir) = step(pos, dir)
    walk(nextPos, nextDir, pos :: walked)

val walked = walk((startRow, startCol), (-1, 0), Nil)
println(walked.toSet.size)
