package com.shabab
package day1.part2

import java.security.InvalidParameterException
import scala.io.Source
import scala.util.Using


object Main extends App {

  private def strToNumber: String => Int = {
    case "one" => 1
    case "two" => 2
    case "three" => 3
    case "four" => 4
    case "five" => 5
    case "six" => 6
    case "seven" => 7
    case "eight" => 8
    case "nine" => 9
    case _ => throw new InvalidParameterException()
  }

  private def getNumberFromSubstring: String => Option[Int] = str => {
    val numericWords = Array("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

    numericWords.find(nw => str.endsWith(nw)).map { nw =>

      strToNumber(nw)
    }
  }

  Using(Source.fromResource("day1_p2.txt")) { source =>

    val result = source.getLines().map { line =>
      line.foldLeft(("", "")) { case ((a, b), x) =>
        val currentSubstring = s"$b$x"

        getNumberFromSubstring(currentSubstring) match {
          case Some(value) => (s"$a$value", x.toString)
          case None if x.isLetter => (a, s"$b$x")
          case _ => (s"$a$x", s"")
        }
      }
    }.map { case (allDigits, _) =>

      s"${allDigits.charAt(0)}${allDigits.charAt(allDigits.length - 1)}"
    }.map(_.toInt).sum

    println(result)
  }
}
