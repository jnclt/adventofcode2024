#!/usr/bin/env -S scala-cli shebang

case class Pos(x: Int, y: Int):
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)
  def *(f: Int) = Pos(this.x * f, this.y * f)
  def %(that: Pos) = Pos(this.x % that.x, this.y % that.y)
  def /(that: Pos) = Pos(this.x / that.x, this.y / that.y)

val dim = Pos(101, 103)
val mid = dim / Pos(2, 2)

def parse(line: String): (Pos, Pos) =
  val s"p=$px,$py v=$vx,$vy" = line: @unchecked
  (Pos(px.toInt, py.toInt), Pos(vx.toInt, vy.toInt))

def at(steps: Int)(from: Pos, vel: Pos): Pos =
  (from + (vel * steps)) % dim

def norm(p: Pos): Pos =
  Pos(
    if p.x >= 0 then p.x else dim.x + p.x,
    if p.y >= 0 then p.y else dim.y + p.y
  )

def quadrant(p: Pos): Option[Pos] =
  if p.x == mid.x || p.y == mid.y then None
  else
    val qx = if p.x < mid.x then 0 else 1
    val qy = if p.y < mid.y then 0 else 1
    Some(Pos(qx, qy))

val robots = io.Source.fromFile("input.txt").getLines.map(parse).toList
val counts = robots
  .map(r => quadrant(norm(at(100).tupled(r))))
  .flatten
  .groupBy(identity)
  .values
  .map(_.size)
println(counts.product)
