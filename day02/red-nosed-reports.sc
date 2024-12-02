#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/2

val reports = io.Source
  .fromFile("input.txt")
  .getLines
  .toVector
  .map(_.split(" ").toVector.map(_.toInt))

def isSafe(report: Vector[Int]): Boolean =
  val steps = report.sliding(2).map(pair => pair(1) - pair(0))
  if report.head < report.last then steps.forall(s => 1 <= s && s <= 3)
  else if report.head > report.last then steps.forall(s => -3 <= s && s <= -1)
  else false

println(reports.count(isSafe))

def isAlmostSafe(report: Vector[Int]): Boolean =
  def omitOneStep(report: Vector[Int]) =
    (0 to report.size - 1).map(report.patch(_, Nil, 1))
  (report +: omitOneStep(report)).exists(isSafe)

println(reports.count(isAlmostSafe))
