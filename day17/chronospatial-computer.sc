#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/17

val lines = io.Source.fromFile("input.txt").getLines
// val lines = io.Source.fromFile("sample.txt").getLines

def parse(): (Int, Int, Int, Vector[Int]) =
  val s"Register A: $ra" = lines.next: @unchecked
  val s"Register B: $rb" = lines.next: @unchecked
  val s"Register C: $rc" = lines.next: @unchecked
  lines.next
  val s"Program: $program" = lines.next: @unchecked
  (
    ra.toInt,
    rb.toInt,
    rc.toInt,
    program.split(",").map(_.toInt).toVector
  )

val (vra, vrb, vrc, program) = parse()

var (ra, rb, rc) = (vra, vrb, vrc)
var pointer = 0

import scala.collection.mutable.ArrayBuffer
val output = ArrayBuffer.empty[Int]

def combo(operand: Int): Option[Int] =
  operand match
    case 4 => Some(ra)
    case 5 => Some(rb)
    case 6 => Some(rc)
    case 7 => None
    case n => Some(n)

def xdv(op: Int): Int = (ra / Math.pow(2, combo(op).get.toDouble)).toInt

def adv(op: Int): Unit = ra = xdv(op)

def bxl(op: Int): Unit = rb = rb ^ op

def bst(op: Int): Unit = rb = combo(op).get % 8

def jnz(op: Int): Unit = if ra != 0 then pointer = op else ()

def bxc(op: Int): Unit = rb = rb ^ rc

def out(op: Int): Unit = output.addOne(combo(op).get % 8)

def bdv(op: Int): Unit = rb = xdv(op)

def cdv(op: Int): Unit = rc = xdv(op)

val ins =
  Map(
    0 -> adv,
    1 -> bxl,
    2 -> bst,
    3 -> jnz,
    4 -> bxc,
    5 -> out,
    6 -> bdv,
    7 -> cdv
  )

while pointer < program.size do
  val ptr = pointer
  val i = program(pointer)
  val op = program(pointer + 1)
  ins(i)(op)
  if pointer == ptr then pointer = pointer + 2 else ()

println(output.mkString(","))
