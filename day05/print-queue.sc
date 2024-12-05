#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/5

val lines = io.Source.fromFile("input.txt").getLines
val rules = lines
  .takeWhile(_.nonEmpty)
  .map(line =>
    val s"$page|$succ" = line: @unchecked
    (page.toInt, succ.toInt)
  )

val predecessors = rules.toList
  .groupBy(_._2)
  .mapValues(_.map(_._1).toSet)
  .toMap

def isValid(update: List[Int]): Boolean =
  val (_, valid) = update.foldLeft((Set.empty[Int], true)) {
    case ((preds: Set[Int], valid: Boolean), page: Int) =>
      (
        preds ++ predecessors.getOrElse(page, Set.empty),
        if !valid then false else !preds.contains(page)
      )
  }
  valid

val updates = lines.map(_.split(',').map(_.toInt).toList)
val score = updates
  .filter(isValid)
  .map(update =>
    val mid = update.size / 2
    update(mid)
  )
  .sum

println(score)
