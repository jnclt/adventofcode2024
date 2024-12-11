#!/usr/bin/env -S scala-cli shebang

val line = io.Source.fromFile("input.txt").getLines.next
val startRow = line.split(' ').map(BigInt(_)).toSeq

def blink(v: BigInt): Seq[BigInt] =
  if v == 0 then Seq(1)
  else if v.toString.size % 2 == 0 then
    val vs = v.toString
    val (ls, rs) = vs.splitAt(vs.size / 2)
    Seq(BigInt(ls), BigInt(rs))
  else Seq(v * 2024)

val last = (1 to 25).foldLeft(startRow)((row, _) => row.flatMap(blink))
println(last.size)

import scala.collection.mutable.Map as mMap

val lengths = mMap[BigInt, mMap[Int, BigInt]]()

def blinkRec(stop: Int)(v: BigInt): BigInt =
  lengths.get(v).flatMap(_.get(stop)) match
    case Some(l) => l
    case None =>
      if !lengths.contains(v) then lengths.update(v, mMap())
      if stop == 0 then
        lengths.update(v, mMap(0 -> BigInt(1)))
        1
      else
        val nexts = blink(v)
        val vLen = nexts.map(blinkRec(stop - 1)).sum
        lengths(v).update(stop, vLen)
        vLen

val counts = startRow.map(blinkRec(75))
println(counts.sum)
