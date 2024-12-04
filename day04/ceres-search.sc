#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/4

val lines = io.Source.fromFile("input.txt").getLines.toVector

def occurrenceCount(strings: Vector[String]): Int =
  strings.map { s =>
    s"XMAS".r.findAllMatchIn(s).size + s"SAMX".r.findAllMatchIn(s).size
  }.sum

def diagonal(strings: Vector[String]): Vector[String] =
  val height = strings.size
  val width = strings.head.size
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
