#!/usr/bin/env -S scala-cli shebang

val lines = io.Source.fromFile("input.txt").getLines
val tokens = lines.next.split(", ").toList
lines.next
val words = lines.toList

import scala.collection.mutable.Map as mMap

val composableCounts = mMap("" -> BigInt(1))

def suffixes(s: String): List[String] =
  tokens.collect { case t: String if s.startsWith(t) => s.drop(t.size) }

def composableCount(s: String): BigInt =
  val sfxs = suffixes(s)
  val (known, unknown) = sfxs.partition(composableCounts.contains)
  val count = known.map(composableCounts).sum + unknown.map(composableCount).sum
  composableCounts(s) = count
  count

tokens.map(composableCount) // init

val possibleCounts = words.map(composableCount)
println(possibleCounts.count(_ > 0))
println(possibleCounts.sum)
