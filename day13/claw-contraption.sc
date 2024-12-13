#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/13

val lines = io.Source
  .fromFile("input.txt")
  .getLines

def parse(machine: Seq[String]): (Int, Int, Int, Int, BigInt, BigInt) =
  val s"Button A: X+$s_xa, Y+$s_ya" = machine(0): @unchecked
  val s"Button B: X+$s_xb, Y+$s_yb" = machine(1): @unchecked
  val s"Prize: X=$x, Y=$y" = machine(2): @unchecked
  (s_xa.toInt, s_ya.toInt, s_xb.toInt, s_yb.toInt, BigInt(x), BigInt(y))

def price(
    s_xa: Int,
    s_ya: Int,
    s_xb: Int,
    s_yb: Int,
    x: BigInt,
    y: BigInt
): BigInt =
// a*s_xa + b*s_xb = x -> a = (x - b*s_xb)/s_xa
// a*s_ya + b*s_yb = y -> b = (y*s_xa - x*s_ya)/(s_yb*s_xa - s_xb*s_ya)
  val bDividend = (y * s_xa - x * s_ya)
  val b = bDividend / (s_yb * s_xa - s_xb * s_ya)
  val aDividend = (x - b * s_xb)
  val a = aDividend / s_xa
  if a * s_xa == aDividend && b * (s_yb * s_xa - s_xb * s_ya) == bDividend then
    (a * 3 + b)
  else 0

val machines = lines.grouped(4).map(parse).toList
println(machines.map(price.tupled).sum)

def bumpup(
    s_xa: Int,
    s_ya: Int,
    s_xb: Int,
    s_yb: Int,
    x: BigInt,
    y: BigInt
): (Int, Int, Int, Int, BigInt, BigInt) =
  (
    s_xa,
    s_ya,
    s_xb,
    s_yb,
    x + BigInt("10000000000000"),
    y + BigInt("10000000000000")
  )

println(machines.map(bumpup).map(price.tupled).sum)
