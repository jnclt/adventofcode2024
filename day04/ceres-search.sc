#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/4

val lines = io.Source.fromFile("input.txt").getLines.toVector
val height = lines.size
val width = lines.head.size

def occurrenceCount(strings: Vector[String]): Int =
  strings.map { s =>
    s"XMAS".r.findAllMatchIn(s).size + s"SAMX".r.findAllMatchIn(s).size
  }.sum

def diagonal(strings: Vector[String]): Vector[String] =
  (0 until height).map { sy =>
    (sy until height).map { y => strings(y)(y - sy) }.mkString
  }.toVector ++
    (1 until width).map { sx =>
      (sx until width).map { x => strings(x - sx)(x) }.mkString
    }.toVector

val total =
  occurrenceCount(lines) + // horizontal
    occurrenceCount(lines.transpose.map(_.mkString)) + // vertical
    occurrenceCount(diagonal(lines)) + // diagonal
    occurrenceCount((diagonal(lines.map(_.reverse)))) // reverse diagonal

println(total)

def isXmasCrosspoint(row: Int, col: Int): Boolean =
  if row < 1 || row >= height - 1 || col < 1 || col >= width - 1 then false
  else if lines(row)(col) != 'A' then false
  else if !((lines(row - 1)(col - 1) == 'M' && lines(row + 1)(col + 1) == 'S') || 
            (lines(row - 1)(col - 1) == 'S' && lines(row + 1)(col + 1) == 'M'))
  then false
  else if !((lines(row - 1)(col + 1) == 'M' && lines(row + 1)(col - 1) == 'S') ||
            (lines(row - 1)(col + 1) == 'S' && lines(row + 1)(col - 1) == 'M'))
  then false
  else true

val total2 = (1 until height - 1).flatMap { row =>
  (1 until width - 1).filter { col =>
    isXmasCrosspoint(row, col)
  }
}.size

println(total2)
