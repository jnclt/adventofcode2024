#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/6

//> using dep org.scala-lang.modules::scala-parallel-collections:1.0.4

import scala.collection.parallel.CollectionConverters._

val lines = io.Source.fromFile("input.txt").getLines.toVector
// val lines = io.Source.fromFile("sample.txt").getLines.toVector
val rowRange = Range(0, lines.size)
val colRange = Range(0, lines.head.size)
val obstacles = (for
  row <- rowRange
  col <- colRange
  if lines(row)(col) == '#'
yield (row, col)).toSet
val startRow = lines.indexWhere(_.contains('^'))
val startCol = lines(startRow).indexOf('^')

type Position = (Int, Int)
type Direction = (Int, Int)

def turnRight(dir: Direction): Direction = dir match
  case (0, 1)  => (1, 0) // > -> v
  case (1, 0)  => (0, -1) // v -> <
  case (0, -1) => (-1, 0) // < -> ^
  case (-1, 0) => (0, 1) // ^ -> >

def step(
    pos: Position,
    dir: Direction,
    obstacles: Set[(Int, Int)]
): (Position, Direction) =
  val ahead = (pos._1 + dir._1, pos._2 + dir._2)
  if obstacles.contains(ahead) then (pos, turnRight(dir))
  else (ahead, dir)

@scala.annotation.tailrec
def walk(
    pos: Position,
    dir: Direction,
    walked: List[Position]
): List[Position] =
  if !(rowRange.contains(pos._1) && colRange.contains(pos._2)) then walked
  else
    val (nextPos, nextDir) = step(pos, dir, obstacles)
    walk(nextPos, nextDir, pos :: walked)

val walked = walk((startRow, startCol), (-1, 0), Nil)
println(walked.toSet.size)

@scala.annotation.tailrec
def containsLoop(
    pos: Position,
    dir: Direction,
    walked: Set[(Position, Direction)],
    obstacles: Set[(Int, Int)]
): Boolean =
  if !(rowRange.contains(pos._1) && colRange.contains(pos._2)) then false
  else
    val next = step(pos, dir, obstacles)
    if walked.contains(next) then true
    else containsLoop(next._1, next._2, walked + next, obstacles)

val withExtraObstacle = (for
  row <- rowRange
  col <- colRange
yield obstacles.incl((row, col))).toSet

val loops = withExtraObstacle.par.filter(
  containsLoop((startRow, startCol), (-1, 0), Set.empty, _)
)
println(loops.size)
