#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/1

import Math.abs

val lines = io.Source.fromFile("input.txt").getLines.toVector
val (leftList, rightList) = lines.map { pair =>
  val (s"$l   $r") = pair: @unchecked
  (l.toInt, r.toInt)
}.unzip

val diffSum =
  leftList.sorted.zip(rightList.sorted).foldLeft(0) { case (sum, (l, r)) =>
    sum + abs(r - l)
  }
println(diffSum)

val occurrences = rightList.groupBy(identity).mapValues(_.size)
val simScore = leftList.foldLeft(0) { (sum, l) =>
  sum + (l * occurrences.getOrElse(l, 0))
}
println(simScore)
