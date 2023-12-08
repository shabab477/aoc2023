package com.shabab
package day6.part2

import scala.io.Source
import scala.util.{Failure, Using}

//noinspection Duplicates
object Main extends App {

  private sealed trait Multiplier
  private case object Positive extends Multiplier
  private object Negative extends Multiplier

  private def solveQuad(b: Long, c: Long, sign: Multiplier)(implicit a: Long = 1): Double = {
    val m = sign match {
      case Positive => 1
      case _ => -1
    }
    val numerator = -1 * b + (Math.sqrt(b * b - 4 * a * c) * m)

    numerator / (2 * a)
  }

  val process = Using(Source.fromResource("day6_p2.txt")) { source =>
    val allLines = source.getLines().toList
    val time = """\d+""".r.findAllIn(allLines.head.split(":")(1)).mkString("").toLong
    val distance = """\d+""".r.findAllIn(allLines.last.split(":")(1)).mkString("").toLong


    println(time)
    println(distance)
    // Solve for equation: x^{2}-t*x+d=0
    // Refer to this graph for solution: https://www.desmos.com/calculator/vlg0otmsma
    val answer = Math.ceil(Math.abs(solveQuad(time, distance, Positive) - solveQuad(time, distance, Negative)))
    println(answer.toLong)
  }

  process match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }

}
