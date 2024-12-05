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
  .view
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
val (valid, invalid) = updates.partition(isValid)

def score(updates: Iterator[List[Int]]): Int =
  updates
    .map(update =>
      val mid = update.size / 2
      update(mid)
    )
    .sum

println(score(valid))

@scala.annotation.tailrec
def reorder(update: List[Int]): List[Int] =
  val validPrefixPreds = update
    .scanLeft((Set.empty[Int], true)) {
      case ((preds: Set[Int], valid: Boolean), page) =>
        val stillValid = valid && !preds.contains(page)
        (preds ++ predecessors.getOrElse(page, Set.empty), stillValid)
    }
    .takeWhile(_._2)
    .map(_._1)

  val offendingIdx = validPrefixPreds.size - 1
  if offendingIdx == update.size then update
  else
    val offendingPage = update(offendingIdx)
    val swappedUpdate = update
      .updated(offendingIdx, update(offendingIdx - 1))
      .updated(offendingIdx - 1, offendingPage)
    reorder(swappedUpdate)

println(score(invalid.map(reorder)))
