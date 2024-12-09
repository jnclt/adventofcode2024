#!/usr/bin/env -S scala-cli shebang

val line = io.Source.fromFile("input.txt").getLines().next()

case class Block(name: Int, len: Int)
val fileBlocks =
  (0 until line.size by 2).zipWithIndex
    .map((s, i) => Block(i, line(s).asDigit))
    .toVector
val gaps = (1 until line.size by 2).map(line(_).asDigit)

@scala.annotation.tailrec
def getFill(
    gap: Int,
    from: Vector[Block],
    fill: Vector[Block]
): (Vector[Block], Vector[Block]) =
  if from.isEmpty then (from, fill)
  else
    val next = from.last
    if gap > next.len then
      getFill(gap - next.len, from.dropRight(1), fill :+ next)
    else if next.len > gap then
      (
        from.dropRight(1) :+ Block(next.name, next.len - gap),
        fill :+ Block(next.name, gap)
      )
    else (from.dropRight(1), fill :+ next)

@scala.annotation.tailrec
def swap(
    gaps: Seq[Int],
    swapped: Vector[Block],
    toSwap: Vector[Block]
): Vector[Block] =
  if toSwap.size <= 1 then swapped ++ toSwap
  else
    val swappedNew = swapped :+ toSwap.head
    val toSwapNew = toSwap.tail
    val (toSwapNext, fill) = getFill(gaps.head, toSwapNew, Vector.empty)
    swap(gaps.tail, swappedNew ++ fill, toSwapNext)

val swapped =
  swap(gaps, Vector.empty, fileBlocks)
def checksum(files: Vector[Block]): BigInt =
  files
    .foldLeft(BigInt(0), BigInt(0)) { case ((sum, idx), block) =>
      (
        sum + (0 until block.len).map(bi => (idx + bi) * block.name).sum,
        idx + block.len
      )
    }
    ._1

println(checksum(swapped))

import collection.mutable.ArrayBuffer

case class File(name: Int, len: Int, gap: Int)
val files =
  ArrayBuffer(
    fileBlocks.zip(gaps.toVector :+ 0).map((b, g) => File(b.name, b.len, g))*
  )

def shuffleFiles(
    fileName: Int,
    shuffled: ArrayBuffer[File]
): ArrayBuffer[File] =
  if fileName == 0 then shuffled
  else
    val nextIdx = shuffled.indexWhere(_.name == fileName)
    val next = shuffled(nextIdx)
    val predIdx = shuffled.take(nextIdx).indexWhere(_.gap >= next.len)
    if predIdx == -1 then shuffled
    else
      val oldPred = shuffled(nextIdx - 1)
      shuffled.update(
        nextIdx - 1,
        oldPred.copy(gap = oldPred.gap + next.len + next.gap)
      )
      shuffled.remove(nextIdx)
      val pred = shuffled(predIdx)
      val newNext = next.copy(gap = pred.gap - next.len)
      shuffled.insert(predIdx + 1, newNext)
      shuffled.update(predIdx, pred.copy(gap = 0))
      shuffled

val shuffledFiles = (1 to files.map(_.name).max).foldRight(files)(shuffleFiles)
val split =
  shuffledFiles.toVector.flatMap(f =>
    Vector(Block(f.name, f.len), Block(0, f.gap))
  )
println(checksum(split))
