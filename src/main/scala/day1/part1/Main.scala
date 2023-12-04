package com.shabab
package day1.part1

import scala.io.Source
import scala.util.Using


object Main extends App {
  Using(Source.fromResource("day1_p1.txt")) { source =>

    val result = source.getLines().map { line =>
      val allDigits = line.filter(_.isDigit)

      s"${allDigits.charAt(0)}${allDigits.charAt(allDigits.length - 1)}"
    }.map(_.toInt).sum

    println(result)
  }
}
