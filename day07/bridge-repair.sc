#!/usr/bin/env -S scala-cli shebang

// https://adventofcode.com/2024/day/7

//> using dep org.scala-lang.modules::scala-parallel-collections:1.0.4

import scala.collection.parallel.CollectionConverters._

case class Equation(
    result: BigInt,
    values: Vector[Int]
)

val lines = io.Source.fromFile("input.txt").getLines
val equations = lines
  .map(l =>
    val (s"$result: $values") = l: @unchecked
    Equation(BigInt(result), values.split(" ").map(_.toInt).toVector)
  )
  .toVector

def hasSolution(res: BigInt, eq: Equation, concat: Boolean): Boolean =
  if eq.values.isEmpty then res == eq.result
  else if res > eq.result then false
  else
    hasSolution(
      res * eq.values.head,
      Equation(eq.result, eq.values.tail),
      concat
    )
    ||
    hasSolution(
      res + eq.values.head,
      Equation(eq.result, eq.values.tail),
      concat
    )
    ||
    (concat && hasSolution(
      BigInt(res.toString() + eq.values.head.toString()),
      Equation(eq.result, eq.values.tail),
      concat
    ))

def total(equations: Vector[Equation], concat: Boolean): BigInt = equations.par
  .filter(eq => hasSolution(0, eq, concat))
  .map(_.result)
  .sum

println(total(equations, concat = false))
println(total(equations, concat = true))
