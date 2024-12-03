#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/3

val code = io.Source
  .fromFile("input.txt")
  .getLines
  .mkString("")

val mulExpr = raw"mul\(([0-9][0-9]?[0-9]?),([0-9][0-9]?[0-9]?)\)".r

def total(text: String): Int =
  mulExpr
    .findAllMatchIn(text)
    .map { matched =>
      matched.group(1).toInt * matched.group(2).toInt
    }
    .sum

println(total(code))

@scala.annotation.tailrec
def totalRec(text: String, acc: Int): Int =
  raw"don't\(\)".r.findFirstMatchIn(text) match
    case None => acc + total(text)
    case Some(dontMatch) =>
      val partialSum = total(dontMatch.before.toString)
      raw"do\(\)".r.findFirstMatchIn(dontMatch.after.toString) match
        case None          => acc + partialSum
        case Some(doMatch) => totalRec(doMatch.after.toString, acc + partialSum)

println(totalRec(code, 0))
