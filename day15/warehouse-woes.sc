#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/15

case class Pos(x: Int, y: Int):
  def +(that: Pos) = Pos(this.x + that.x, this.y + that.y)
  lazy val gps = this.y * 100 + this.x

val lines = io.Source.fromFile("input.txt").getLines
val grid = lines.takeWhile(!_.isBlank).toList
val moves = lines.mkString.toCharArray()

val rowSpan = 0 until grid.size
val colSpan = 0 until grid.head.size

val walls = (for
  y <- rowSpan
  x <- colSpan
  if grid(y)(x) == '#'
yield Pos(x, y)).toSet

val boxes0 = (for
  y <- rowSpan
  x <- colSpan
  if grid(y)(x) == 'O'
yield Pos(x, y)).toSet

val sy = grid.indexWhere(_.find(_ == '@').isDefined)
val sx = grid(sy).indexWhere(_ == '@')
val start = Pos(sx, sy)

case class State(at: Pos, boxes: Set[Pos])

def next(from: Pos, move: Char): Option[Pos] =
  val ahead = move match
    case '>' => from + Pos(+1, 0)
    case '<' => from + Pos(-1, 0)
    case 'v' => from + Pos(0, +1)
    case '^' => from + Pos(0, -1)
  if walls(ahead) then None else Some(ahead)

def step(box: Boolean)(from: State, move: Char): State =
  next(from.at, move) match
    case None => from
    case Some(n) if !from.boxes(n) =>
      if box then State(n, from.boxes - from.at + n)
      else from.copy(at = n)
    case Some(n) =>
      val boxAheadNextState = step(true)(from.copy(at = n), move)
      if boxAheadNextState.at == n then from
      else if box then State(n, boxAheadNextState.boxes - from.at + n)
      else boxAheadNextState.copy(at = n)

val endState = moves.foldLeft(State(start, boxes0))(step(false))
println(endState.boxes.map(_.gps).sum)
