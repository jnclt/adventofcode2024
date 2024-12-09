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
val checksum = swapped.foldLeft(BigInt(0), BigInt(0)) {
  case ((sum, idx), block) =>
    (
      sum + (0 until block.len).map(bi => (idx + bi) * block.name).sum,
      idx + block.len
    )
}

println(checksum._1)
