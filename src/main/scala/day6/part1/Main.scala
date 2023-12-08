package com.shabab
package day6.part1

import scala.io.Source
import scala.util.{Failure, Using}

//noinspection Duplicates
object Main extends App {

  val process = Using(Source.fromResource("day6_p1.txt")) { source =>
    val allLines = source.getLines().toList
    val times = """\d+""".r.findAllIn(allLines.head.split(":")(1)).map(_.toLong)
    val distances = """\d+""".r.findAllIn(allLines.last.split(":")(1)).map(_.toLong)

    val input = times.zip(distances)
    val winningCounts = input.toList.map { case (time, distance) =>

      Seq.range(0, time + 1, 1).count { speed =>
        val leftoverTime = time - speed
        val distanceCovered = speed * leftoverTime

        distanceCovered > distance
      }
    }

    println(winningCounts.product)
  }

  process match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }

}
